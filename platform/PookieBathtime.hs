{-# LANGUAGE RecursiveDo, TupleSections #-}
module PookieBathtime {- (pookieBathtime) -} where

import Page
import qualified Level1
import Level
import SimpleElement

import FRP.Sodium
import FRP.Sodium.GameEngine2D.Geometry
import FRP.Sodium.GameEngine2D.Gesture
import FRP.Sodium.GameEngine2D.Platform

import Control.Applicative
import Control.Arrow (first)
import Data.Array (Array)
import qualified Data.Array.IArray as A
import Data.Array.IArray ((!))
import Data.Default
import qualified Data.IntMap as IM
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Ord
import Data.Traversable
import Debug.Trace
import System.Random


data Resources p = Resources {
        rsGameBG       :: Sprite p,
        rsPookieTitle  :: Sprite p,
        rsPlayButton   :: Drawable p,
        rsPlayer       :: Array Int (Drawable p),
        rsWomanWithDog :: Sprite p,
        rsDrawElement  :: Element -> Drawable p,
        rsRed          :: Drawable p
    }

pookieBathtime :: Platform p => IO (GameInput p -> Reactive (GameOutput p))
pookieBathtime = game <$> loadResources <*> newStdGen

loadResources :: Platform p => IO (Resources p)
loadResources = Resources
    <$> backgroundImage "background.jpg"
    <*> backgroundImage "pookie-title.jpg"
    <*> image "play-button.png"
    <*> (do
        images <- forM [1..5::Int] $ \i -> image $ "copyrighted-running-dog-" <> show i <> ".png"
        return $ A.listArray (0, length images-1) images
    )
    <*> backgroundImage "woman-with-dog.jpg"
    <*> loadElementResources
    <*> image "red.png"

titlePage :: Platform p =>
             Resources p
          -> StdGen
          -> Page p
titlePage res rng0 = Page $ \gi -> do
    (eClick, held) <- clickGesture (pure (`inside` buttonRectUp)) (giMouse gi)
    return (
        def {
            goSprite = (rsPookieTitle res <>) . mconcat <$> sequenceA [
                   (\held -> rsPlayButton res (if held then buttonRectDown else buttonRectUp)) <$> held
               ]
        },
        const (introPage res rng0) <$> eClick
      )
  where
    buttonRectUp   = ((0,-400),(100,100))
    buttonRectDown = first (`plus` (5,0)) buttonRectUp

introPage :: Platform p =>
             Resources p
          -> StdGen
          -> Page p
introPage res rng0 = Page $ \gi -> do
    t0 <- sample (giTime gi)
    let eEnd = filterJust $ fmap (\t -> if t - t0 >= 0.5 then Just $ gamePage res levels rng0 else Nothing)
                   (updates $ giTime gi)
    return (
        def {
            goSprite = pure $ rsWomanWithDog res
        },
        eEnd
      )
  where
    levels = [
         Level1.level
      ]

gameSpeed :: Float
gameSpeed = 400

removeDuplicates :: Eq a => Behavior a -> Reactive (Behavior a)
removeDuplicates ba = do
    a0 <- sample ba
    hold a0 $ filterJust $ snapshot (\new old ->
            if new /= old then Just new
                          else Nothing
        ) (updates ba) ba

gamePage :: Platform p =>
            Resources p
         -> [Level Element]
         -> StdGen
         -> Page p
gamePage res (level0:levels0) rng0 = Page $ \GameInput { giTime = time, giAspect = aspect } -> do
    t0 <- sample time
    x0 <- ((-800) *) <$> sample aspect
    rec
        let xOrig = (\t -> x0 - (realToFrac (t - t0) * gameSpeed)) <$> time
            yOrig = negate <$> yPos
            orig = liftA2 (,) xOrig yOrig
        ixRange <- removeDuplicates $ liftA2 (\aspect xOrig ->
                    let xExtent = 1000 * aspect + levelScale * 0.5
                        minX = (-xOrig) - xExtent
                        maxX = (-xOrig) + xExtent
                        minIx = floor $ minX / levelSpacing
                        maxIx = ceiling  $ maxX / levelSpacing
                    in  (minIx, maxIx)
                ) aspect xOrig
        let terr = (\(minIx, maxIx) -> -- trace ("range "++show minIx++" - "++show maxIx) $
                    let (_, terr') = IM.split (minIx-1) (leTerrain level0)
                        (terr'', _) = IM.split (maxIx+1) terr'
                    in  terr''
                ) <$> ixRange
            levelSpr = liftA2 (drawTerrain (rsDrawElement res)) orig terr
        (plSpr, yPos) <- player res aspect xOrig yOrig time never
    return (
        def {
            goSprite = mconcat <$> sequenceA [
                pure $ rsGameBG res,
                levelSpr,
                plSpr
              ]
        },
        never
      )

closeTo :: (Ord a, Fractional a) => a -> a -> Bool
closeTo a b = abs (a - b) < 0.000001

interpolate :: RealFloat a => (a,a) -> (a,a) -> a -> a
interpolate (t0, x0) (t1, x1) t =
    if t0 `closeTo` t1
        then (x0 + x1) * 0.5
        else (t - t0) * (x1 - x0) / (t1 - t0) + x0

foldlNeighbours :: (a -> b -> b -> a) -> a -> [b] -> a
foldlNeighbours f start xs =
    foldl' (\a -> uncurry (f a)) start $ zip (last xs:xs) xs

-- | True if the specified point is inside the polygon (inclusive)
insidePolygon :: RealFloat a => (a,a) -> [(a,a)] -> Bool
-- Pathological case: Point on vertex - count as inside
insidePolygon pt vertices | any (== pt) vertices = True
-- Normal case - inside if odd number of intersections with ray
insidePolygon (x, y) vertices = foldlNeighbours consider False vertices
    where
        consider maybeInside (x0, y0) (x1, y1) =
            let betweenYs = y >= y0 && y < y1 ||
                            y >= y1 && y < y0
            in  if (betweenYs && x < interpolate (y0, x0) (y1, x1) y)
                then not maybeInside
                else maybeInside

solveLinear :: RealFloat a => (a,a) -> [a]
solveLinear (0,b) = [] -- constant function has no roots
solveLinear (a,b) = [-b/a]

solveQuadratic :: RealFloat a => (a,a,a) -> [a]
solveQuadratic (0,b,c) = solveLinear (b,c)
solveQuadratic (a,b,c) =
    let discr = b ^ 2 - 4 * a * c
    in  if discr < 0
            then []
            else
                let sqrtDiscr = sqrt discr
                    twoa = 2 * a
                    x1 = ((-b) - sqrtDiscr) / twoa
                    x2 = ((-b) + sqrtDiscr) / twoa
                in  [x1, x2]

differential :: RealFloat a => (a, a, a) -> (a, a)
differential (a,b,c) = (2*a,b)

calculateQuadratic :: RealFloat a => (a,a,a) -> a -> a
calculateQuadratic (a,b,c) x = a*x^2 + b*x + c

calculateLinear :: RealFloat a => (a,a) -> a -> a
calculateLinear (a,b) x = a*x + b

-- | Add a constant to a quadratic
offsetQuadratic :: RealFloat a => a -> (a,a,a) -> (a, a, a)
offsetQuadratic c' (a, b, c) = (a, b, c + c')

gravity :: Coord
gravity = -1000

-- | Return a list of collisions of points
intersections :: Float -> Float -> (Float, (Float, Float, Float)) -> Float -> Float -> [((Int,Int),Point)]
intersections spacing radius (x0, q) xa xb =
    let (p0,p1) = wings xa
        (p2,p3) = wings xb
        poly = [p0,p2,p3,p1]
        xs = map fst poly
        ys = map snd poly
        xi0 = floor (minimum xs / spacing) :: Int
        xi1 = ceiling (maximum xs / spacing) :: Int
        yi0 = floor (minimum ys / spacing) :: Int
        yi1 = ceiling (maximum ys / spacing) :: Int
        all_xyis = [(xi,yi) | xi <- [xi0..xi1], yi <- [yi0..yi1]]
        matched_xyis = mapMaybe (\xyi@(xi,yi) ->
            let xy = (fromIntegral xi * spacing, fromIntegral yi * spacing) 
            in  if insidePolygon xy poly
                        then Just (xyi, xy)
                        else Nothing
          ) all_xyis
    in  matched_xyis
  where
    wings x =
        let dx = x - x0
            y = calculateQuadratic q dx
            slope = calculateLinear (differential q) dx
            (vx, vy) = radius `scale` normalize (slope, 1)
        in  ((x0 + dx - vx, y + vy),(x0 + dx + vx, y - vy))

splitWhile :: (a -> Bool) -> [a] -> ([a], [a])
splitWhile pred xs = go [] xs
  where
    go acc (x:xs) | pred x = go (x:acc) xs
    go acc xs              = (reverse acc, xs)

player :: Platform p =>
          Resources p
       -> Behavior Float           -- ^ Aspect ratio
       -> Behavior Float           -- ^ Screen X position of game world origin
       -> Behavior Float           -- ^ Screen Y position of game world origin
       -> Behavior Double
       -> Event ()                 -- ^ Jump
       -> Reactive (Behavior (Sprite p), Behavior Coord)
player res aspect xOrig yOrig time eJump = do
    aspect0 <- sample aspect
    let xPos = (\xOrig -> -xOrig - 800 * aspect0) <$> xOrig
    x0 <- sample xPos
    t0 <- sample time

    signal0 <- hold (x0, (gravity/gameSpeed^2, 1000/gameSpeed, 700)) never   -- ^ t0 and quadratic of current jump

    let collisions = snapshot (\xPos' (xPos, x0q) ->
              intersections levelSpacing levelSpacing x0q xPos xPos'
          ) (updates xPos) ((,) <$> xPos <*> signal0)
    collisionPoses <- accum mempty $ ((<>) . snd) <$> collisions
    let collisionSprs = liftA3 (\xys xOrig yOrig ->
              mconcat $ map (rsRed res . (,(levelSpacing,levelSpacing)) . plus (xOrig, yOrig)) xys
          ) collisionPoses xOrig yOrig

    let yPos = liftA2 (\(x0, q) x ->
            let dx = realToFrac (x - x0)
            in  calculateQuadratic q dx
          ) signal0 xPos
        character = (\t ->
            let ix = floor $ nFrames * snd (properFraction (2 * (t - t0)))
            in  rsPlayer res ! ix
          ) <$> time
        sprite = (\draw xOrig yOrig xPos yPos ->
            draw ((xOrig, yOrig) `plus` (xPos, yPos),(levelScale,levelScale))
          ) <$> character <*> xOrig <*> yOrig <*> xPos <*> yPos
    return (liftA2 mappend collisionSprs sprite, yPos)
  where
    nFrames = realToFrac . succ . snd . A.bounds . rsPlayer $ res

game :: Platform p =>
        Resources p
     -> StdGen
     -> GameInput p
     -> Reactive (GameOutput p)
game res rng gi =
    pager (titlePage res rng) gi

