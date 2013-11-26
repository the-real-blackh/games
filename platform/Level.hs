module Level where

import Data.Default
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM

data Level e = Level {
        leTerrain :: IntMap [(Int, e)]
    }
    deriving (Read, Show)

instance Default (Level e) where
    def = Level (IM.empty)
