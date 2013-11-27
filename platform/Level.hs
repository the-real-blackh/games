module Level where

import SimpleElement

import Data.Default
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.Monoid
import FRP.Sodium.GameEngine2D.Geometry
import FRP.Sodium.GameEngine2D.Platform


data Level e = Level {
        leTerrain :: IntMap [(Int, e)]
    }
    deriving (Read, Show)

instance Default (Level e) where
    def = Level (IM.empty)

-- | The size of a single terrain unit in Sodium2D co-ordinate space.
levelScale :: Coord
levelScale = 90

placeElement :: Vector -> (Int, Int) -> Rect
placeElement (x0, y0) (x, y) =
    (
        (
            x0 + ((fromIntegral x + 0.5) * levelScale),
            y0 + ((fromIntegral y + 0.5) * levelScale)
        ),
        (levelScale*0.5, levelScale*0.5)
    )

drawTerrain :: Platform p =>
               (e -> Drawable p) -> Vector -> IntMap [(Int, e)] -> Sprite p
drawTerrain render origin terrain =
    mconcat $ map draw items
  where
    items = concatMap flatten . IM.toList $ terrain
    flatten (x, ys) = map (\(y, e) -> (((x, y), e))) ys
    draw (pos, e) = render e (place pos)
    place = placeElement origin

