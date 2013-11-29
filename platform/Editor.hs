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
import System.IO.Error
import Debug.Trace


data Resources p = Resources {
        rsConcrete :: Drawable p,
        rsRed      :: Drawable p
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
            (go, eSaveLevel) <- sync $ editIt renderElt res (level0 :: Level e) gi
            kill <- sync $ listen eSaveLevel $ \level -> do
                let text = C.pack (show level)
                putStrLn $ "saving to "++fn
                C.writeFile fn text
            run go
            kill
        _ -> fail $ "bad data format in " <> fn

elementSelector :: (Eq e, Platform p) =>
                   (e -> Drawable p)
                -> Resources p
                -> Behavior (Maybe e)
                -> e
                -> UIWidget p (Event e)
elementSelector render res active e = widget' (pad (10,10) $ atCentre ## atCentre) $ \prect rect eMouse -> do
    eClick <- clickGesture (flip inside <$> rect) eMouse
    let eltSpr = render e <$> rect
        highlightSpr = liftA2 (\active rect -> if active == Just e then rsRed res rect else mempty)
            active prect
    return $ (const e <$> eClick, UIOutput (liftA2 mappend highlightSpr eltSpr) never, pure (levelScale, levelScale))

elementBar :: forall p e . (Platform p, Eq e, Bounded e, Enum e) =>
              (e -> Drawable p)
           -> Resources p
           -> Behavior (Maybe e)
           -> UIWidget p (Event e)
elementBar render res active =
    let widgets = map (elementSelector render res active) [minBound..maxBound]
    in  backdrop (rsConcrete res) $ flow Vertical $ mconcat widgets

editIt :: (Platform p, Eq e, Enum e, Bounded e) =>
          (e -> Drawable p)
       -> Resources p
       -> Level e
       -> GameInput p
       -> Reactive (
              GameOutput p,
              Event (Level e)
          )
editIt renderElt res level0 GameInput { giAspect = aspect, giMouse = eMouse } = do
    rec
        (dragVec, eDropVec) <- dragGesture everywhere eMouse
        posReal <- accum (0,0) $ plus <$> eDropVec
        let pos = liftA3 maybe posReal (plus <$> posReal) dragVec
    let terrainSpr = fmap (\pos -> drawTerrain renderElt pos (leTerrain level0)) pos
    rec
        elementSel <- hold Nothing (Just <$> eElementSel)
        (eElementSel, UIOutput barSpr barSnd) <- reify
            ((\aspect -> ((-900 * aspect, 900), (0, 0))) <$> aspect)
            eMouse (elementBar renderElt res elementSel)
    return (
        def {
            goSprite = mconcat <$> sequenceA [
                terrainSpr,
                barSpr
              ],
            goEffects = barSnd
        },
        fmap (const level0) eElementSel
      )
  where
    everywhere = pure (const True)

