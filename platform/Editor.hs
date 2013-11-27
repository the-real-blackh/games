{-# LANGUAGE ScopedTypeVariables, RecursiveDo #-}
module Editor where

import Level

import Control.Applicative
import Control.Exception
import Data.Default
import Data.Monoid
import FRP.Sodium.GameEngine2D.Geometry
import FRP.Sodium.GameEngine2D.Gesture
import FRP.Sodium.GameEngine2D.Platform
import FRP.Sodium
import FRP.Sodium.IO
import qualified Data.ByteString.Char8 as C
import System.IO.Error


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
            (go, eSaveLevel) <- sync $ editIt renderElt (level0 :: Level e) gi
            kill <- sync $ listen eSaveLevel $ \level -> do
                let text = C.pack (show level)
                putStrLn $ "saving to "++fn
                C.writeFile fn text
            run go
            kill
        _ -> fail $ "bad data format in " <> fn

editIt :: Platform p =>
          (e -> Drawable p)
       -> Level e
       -> GameInput p
       -> Reactive (
              GameOutput p,
              Event (Level e)
          )
editIt renderElt level0 GameInput { giAspect = aspect, giMouse = eMouse } = do
    rec
        (dragVec, dropVec) <- dragGesture everywhere eMouse
        posReal <- accum (0,0) $ plus <$> dropVec
        let pos = liftA3 maybe posReal (plus <$> posReal) dragVec
    let terrainSpr = fmap (\pos -> drawTerrain renderElt pos (leTerrain level0)) pos
    eClick <- clickGesture everywhere eMouse
    return (def { goSprite = terrainSpr }, fmap (const level0) eClick )
  where
    everywhere = pure (const True)
