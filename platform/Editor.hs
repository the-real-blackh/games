{-# LANGUAGE ScopedTypeVariables #-}
module Editor where

import Level

import Control.Applicative
import Control.Exception
import Data.Default
import Data.Monoid
import FRP.Sodium.GameEngine2D.Geometry
import FRP.Sodium.GameEngine2D.Platform
import FRP.Sodium
import FRP.Sodium.IO
import qualified Data.ByteString.Char8 as C
import System.IO.Error


editor :: forall p e . (Platform p, Read e, Show e) =>
          e
       -> FilePath
       -> GameInput p
       -> (GameOutput p -> IO ())
       -> IO ()
editor _ fn gi run = do
    rLevel0 <- (reads . C.unpack <$> C.readFile fn)
      `catch` \e -> do
        if isDoesNotExistError e
            then return [(def, "")]
            else throwIO e
    case rLevel0 of
        [(level0, "")] -> do
            (go, eSaveLevel) <- sync $ editIt (level0 :: Level e) gi
            kill <- sync $ listen eSaveLevel $ \level -> do
                let text = C.pack (show level)
                C.writeFile fn text
            run go
            kill
        _ -> fail $ "bad data format in " <> fn

editIt :: Platform p =>
          Level e
       -> GameInput p
       -> Reactive (
              GameOutput p,
              Event (Level e)
          )
editIt level0 gi = do
    return (def, never)

