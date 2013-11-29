{-# LANGUAGE ScopedTypeVariables, RecursiveDo #-}
module Editor where

import Level
import UIWidget

import Control.Applicative
import Control.Exception
import Data.Default
import Data.Monoid
import Data.Traversable (sequenceA)
import FRP.Sodium.GameEngine2D.Geometry
import FRP.Sodium.GameEngine2D.Gesture
import FRP.Sodium.GameEngine2D.Platform
import FRP.Sodium
import FRP.Sodium.IO
import qualified Data.ByteString.Char8 as C
import qualified System.IO.Cautious as Cautious
import System.IO.Error
import Debug.Trace


data Resources p = Resources {
        rsConcrete :: Drawable p,
        rsRed      :: Drawable p,
        rsSave     :: Drawable p
    }

editor :: forall p e . (Platform p, Eq e, Read e, Show e, Bounded e, Enum e) =>
          (e -> Drawable p)
       -> FilePath
       -> GameInput p
       -> (GameOutput p -> IO ())
       -> IO ()
editor renderElt fn gi run = do
    rLevel0 <- (reads . C.unpack <$> C.readFile fn)
      `catch` \e -> do
        if isDoesNotExistError e
            then return [(def, "")]
            else throwIO e
    case rLevel0 of
        [(level0, "")] -> do
            res <- Resources <$> image "concrete.png"
                             <*> image "red.png"
                             <*> image "save.png"
            (go, eSaveLevel) <- sync $ editIt renderElt res (level0 :: Level e) gi
            kill <- sync $ listen eSaveLevel $ \level -> do
                putStrLn $ "saving to "++fn
                Cautious.writeFile fn (show level)
            run go
            kill
        _ -> fail $ "bad data format in " <> fn

panelPlacement = pad (10, 10) $ atCentre ## atCentre

elementSelector :: (Eq e, Platform p) =>
                   (e -> Drawable p)
                -> Resources p
                -> Behavior (Maybe e)
                -> e
                -> UIWidget p (Event e)
elementSelector render res active e = widget' panelPlacement $ \prect rect eMouse -> do
    eClick <- clickGesture (flip inside <$> rect) eMouse
    let eltSpr = render e <$> rect
        highlightSpr = liftA2 (\active rect -> if active == Just e then rsRed res rect else mempty)
            active prect
    return (const e <$> eClick, UIOutput (liftA2 mappend highlightSpr eltSpr) never, pure (levelScale, levelScale))

button :: Platform p =>
          Drawable p
       -> UIWidget p (Event ())
button draw = widget panelPlacement $ \rect eMouse -> do
    eClick <- clickGesture (flip inside <$> rect) eMouse
    return (const () <$> eClick, UIOutput (draw <$> rect) never, pure (levelScale, levelScale))

elementBar :: forall p e . (Platform p, Eq e, Bounded e, Enum e) =>
              (e -> Drawable p)
           -> Resources p
           -> Behavior (Maybe e)
           -> UIWidget p (Event (), Event e)
elementBar render res active =
    let eleTypes = map (elementSelector render res active) [minBound..maxBound]
        save = button (rsSave res)
    in  backdrop (rsConcrete res) $ flow Vertical $ liftA2 (,) save (mconcat eleTypes)

editIt :: (Platform p, Eq e, Enum e, Bounded e) =>
          (e -> Drawable p)
       -> Resources p
       -> Level e
       -> GameInput p
       -> Reactive (
              GameOutput p,
              Event (Level e)
          )
editIt renderElt res level0 GameInput { giAspect = aspect, giMouse = eMouse, giTime = time } = do
    rec
        (dragVec, eDropVec) <- dragGesture everywhere eMouse
        posReal <- accum (0,0) $ plus <$> eDropVec
        let pos = liftA3 maybe posReal (plus <$> posReal) dragVec
    rec
        elementSel <- hold Nothing (Just <$> eElementSel)
        ((eSave, eElementSel), UIOutput barSpr barSnd, panelRect) <- reify
            ((\aspect -> ((-900 * aspect, 900), (0, 0))) <$> aspect)
            eMouse (elementBar renderElt res elementSel)
    let notInPanel = (\r pt -> not $ inside pt r) <$> panelRect
    (eDel0, eMouse') <- doubleClickGesture notInPanel eMouse time
    eAdd0 <- clickGesture notInPanel eMouse'
    let eAdd = snapshotWith (\pt (pos, me) ->
                case me of
                    Just e -> insertLevel (findElementClick pos pt) e
                    Nothing -> id
            ) eAdd0 (liftA2 (,) pos elementSel)
    let eDel = snapshotWith (\pt pos ->
                 deleteLevel (findElementClick pos pt)
             ) eDel0 pos
    rec
        level <- accum level0 (eAdd <> eDel)
        let terrainSpr = liftA2 (\pos level -> drawTerrain renderElt pos (leTerrain level)) pos level
    return (
        def {
            goSprite = mconcat <$> sequenceA [
                terrainSpr,
                barSpr
              ],
            goEffects = barSnd
        },
        snapshot eSave level
      )
  where
    everywhere = pure (const True)

