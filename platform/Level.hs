module Level where

import SimpleElement

import Control.Applicative
import Control.Monad
import Data.Default
import Data.IntMap (IntMap)
import qualified Data.IntMap as IM
import Data.List
import Data.Maybe
import Data.Monoid
import FRP.Sodium.GameEngine2D.Geometry
import FRP.Sodium.GameEngine2D.Platform


data Level e = Level {
        leTerrain :: IntMap [(Int, e)]
    }
    deriving (Read, Show)

lookupTerrain :: (Int, Int) -> IntMap [(Int, e)] -> Maybe e
lookupTerrain (xi, yi) terr = join $ (xi `lookup`) <$> yi `IM.lookup` terr

instance Enum e => Default (Level e) where
    def = Level $ IM.fromList [(0, [(0, toEnum 0)])]

insertLevel :: (Int, Int) -> e -> Level e -> Level e
insertLevel (x, y) e le = le {
        leTerrain = IM.alter (Just . add . fromMaybe []) x (leTerrain le)
    }
  where
    add items = (y, e) : filter (\(y', _) -> y' /= y) items

deleteLevel :: (Int, Int) -> Level e -> Level e
deleteLevel (x, y) le = le {
        leTerrain = IM.update del x (leTerrain le)
    }
  where
    del items = case filter (\(y', _) -> y' /= y) items of
        [] -> Nothing
        items' -> Just items'

-- | The size of a single terrain unit in Sodium2D co-ordinate space.
levelScale :: Coord
levelScale = levelSpacing * 2

-- | The size of a single terrain unit in Sodium2D co-ordinate space.
levelSpacing :: Coord
levelSpacing = 100

placeElement :: Vector       -- ^ Scroll origin
             -> (Int, Int)   -- ^ Element co-ordinate
             -> Rect
placeElement (x0, y0) (x, y) =
    (
        (
            x0 + (fromIntegral x * levelSpacing),
            y0 + (fromIntegral y * levelSpacing)
        ),
        (levelScale*0.5, levelScale*0.5)
    )

findElementClick :: Vector     -- ^ Scroll origin
                 -> Point      -- ^ Mouse click position
                 -> (Int, Int)
findElementClick (x0, y0) (mx, my) =
    let x = mx - x0
        y = my - y0
    in  (floor (x / levelScale - 0.5), floor (y / levelScale - 0.5))

drawTerrain :: Platform p =>
               (e -> Drawable p) -> Vector -> IntMap [(Int, e)] -> Sprite p
drawTerrain render origin terrain =
    mconcat $ map draw items
  where
    items = concatMap flatten . IM.toList $ terrain
    flatten (x, ys) = map (\(y, e) -> (((x, y), e))) ys
    draw (pos, e) = render e (place pos)
    place = placeElement origin

