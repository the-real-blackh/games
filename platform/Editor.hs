{-# LANGUAGE ScopedTypeVariables, RecursiveDo #-}
module Editor where

import Level
import UIWidget

import Control.Applicative
import Control.Exception
import Control.Monad
import Data.Char
import Data.Default
import Data.List
import Data.Monoid
import Data.Traversable (sequenceA)
import FRP.Sodium.GameEngine2D.Geometry
import FRP.Sodium.GameEngine2D.Gesture
import FRP.Sodium.GameEngine2D.Platform
import FRP.Sodium
import FRP.Sodium.IO
import qualified Data.ByteString.Char8 as C
import qualified System.IO.Cautious as Cautious
import System.FilePath
import System.IO.Error
import Debug.Trace


data Resources p = Resources {
        rsBackground :: Sprite p,
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
    rLevel0 <- do
        txt0 <- C.unpack <$> C.readFile fn
        unless (heading `isPrefixOf` txt0) $ fail $ "bad heading format in " <> fn
        let txt = reverse . dropWhile isSpace . reverse $ drop (length heading) txt0
        return $ reads txt
      `catch` \e -> do
        if isDoesNotExistError e
            then return [(def, "")]
            else throwIO e
    case rLevel0 of
        [(level0, "")] -> do
            res <- Resources <$> backgroundImage "background.jpg"
                             <*> image "concrete.png"
                             <*> image "red.png"
                             <*> image "save.png"
            (go, eSaveLevel) <- sync $ editIt renderElt res (level0 :: Level e) gi
            kill <- sync $ listen eSaveLevel $ \level -> do
                putStrLn $ "saving to "++fn
                Cautious.writeFile fn (heading <> show level)
            run go
            kill
        _ -> fail $ "bad data format in " <> fn 
  where
    moduleName = dropExtension . takeFileName $ fn
    heading = "module " <> moduleName <> " where\n\n" <>
              "import Level\n" <>
              "import SimpleElement\n" <>
              "import Data.IntMap (fromList)\n\n" <>
              "level = "

panelPlacement = pad (10, 10) $ atCentre ## atCentre

elementBarItemSize :: Vector
elementBarItemSize = (100,100)

elementSelector :: (Eq e, Platform p) =>
                   (e -> Drawable p)
                -> Resources p
                -> Behavior (Maybe e)
                -> e
                -> UIWidget p (Event e)
elementSelector render res active e = widget' panelPlacement $ \prect rect eMouse -> do
    (eClick, _) <- clickGesture (flip inside <$> rect) eMouse
    let eltSpr = render e <$> rect
        highlightSpr = liftA2 (\active rect -> if active == Just e then rsRed res rect else mempty)
            active prect
    return (const e <$> eClick, UIOutput (liftA2 mappend highlightSpr eltSpr) never, pure elementBarItemSize)

button :: Platform p =>
          Resources p
       -> Drawable p
       -> UIWidget p (Event ())
button res draw = widget panelPlacement $ \rect eMouse -> do
    (eClick, down) <- clickGesture (flip inside <$> rect) eMouse
    let spr = liftA2 (\down rect ->
                (if down then rsRed res rect else mempty) <> draw rect
            ) down rect
    return (const () <$> eClick, UIOutput spr never, pure elementBarItemSize)

elementBar :: forall p e . (Platform p, Eq e, Bounded e, Enum e) =>
              (e -> Drawable p)
           -> Resources p
           -> Behavior (Maybe e)
           -> UIWidget p (Event (), Event e)
elementBar render res active =
    let eleTypes = map (elementSelector render res active) [minBound..maxBound]
        save = button res (rsSave res)
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
    (eAdd0, _) <- clickGesture notInPanel eMouse'
    let eAdd = snapshot (\pt (pos, me) ->
                case me of
                    Just e -> insertLevel (findElementClick pos pt) e
                    Nothing -> id
            ) eAdd0 (liftA2 (,) pos elementSel)
    let eDel = snapshot (\pt pos ->
                 deleteLevel (findElementClick pos pt)
             ) eDel0 pos
    rec
        level <- accum level0 (eAdd <> eDel)
        let terrainSpr = liftA2 (\pos level -> drawTerrain renderElt pos (leTerrain level)) pos level
    return (
        def {
            goSprite = mconcat <$> sequenceA [
                pure (rsBackground res),
                terrainSpr,
                barSpr
              ],
            goEffects = barSnd
        },
        snapshot (flip const) eSave level
      )
  where
    everywhere = pure (const True)

