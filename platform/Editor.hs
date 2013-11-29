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
        rsConcrete :: Drawable p
    }

editor :: forall p e . (Platform p, Read e, Show e, Bounded e, Enum e) =>
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
            (go, eSaveLevel) <- sync $ editIt renderElt res (level0 :: Level e) gi
            kill <- sync $ listen eSaveLevel $ \level -> do
                let text = C.pack (show level)
                putStrLn $ "saving to "++fn
                C.writeFile fn text
            run go
            kill
        _ -> fail $ "bad data format in " <> fn

elementSelector :: (Show e, Platform p) =>
                   (e -> Drawable p)
                -> Resources p
                -> e
                -> UIWidget p (Event e)
elementSelector render res elt = widget (pad (show elt) (20,20) $ atCentre ## atCentre) $ \rect eMouse -> do
    eClick <- clickGesture (flip inside <$> rect) eMouse
    return $ (const elt <$> eClick, UIOutput (render elt <$> rect) never, pure (levelScale, levelScale))

elementBar :: forall p e . (Platform p, Show e, Bounded e, Enum e) =>
              (e -> Drawable p)
           -> Resources p
           -> UIWidget p (Event e)
elementBar render res =
    let widgets = map (elementSelector render res) [minBound..maxBound]
    in  backdrop (rsConcrete res) $ flow Vertical $ mconcat widgets

editIt :: (Platform p, Show e, Enum e, Bounded e) =>
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
    (eElementSel, UIOutput barSpr barSnd) <- reify
        ((\aspect -> ((-900 * aspect, 900), (0, 0))) <$> aspect)
        eMouse (elementBar renderElt res)
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

