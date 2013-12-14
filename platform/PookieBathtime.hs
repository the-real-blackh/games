module PookieBathtime (pookieBathtime) where

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
import Data.Monoid
import Data.Traversable
import Debug.Trace
import System.Random


data Resources p = Resources {
        rsGameBG       :: Sprite p,
        rsPookieTitle  :: Sprite p,
        rsPlayButton   :: Drawable p,
        rsPlayer       :: Array Int (Drawable p),
        rsWomanWithDog :: Sprite p,
        rsDrawElement  :: Element -> Drawable p
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
    let eEnd = filterJust $ fmap (\t -> if t - t0 >= 3 then Just $ gamePage res levels rng0 else Nothing)
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
    let xOrig = (\t -> x0 - (realToFrac (t - t0) * gameSpeed)) <$> time
        yOrig = pure 0
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
    plSpr <- player res aspect orig time never
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

player :: Platform p =>
          Resources p
       -> Behavior Float           -- ^ Aspect ratio
       -> Behavior (Float, Float)  -- ^ Screen X position of game world origin
       -> Behavior Double
       -> Event ()            -- ^ Jump
       -> Reactive (Behavior (Sprite p))
player res aspect orig time eJump = do
    t0 <- sample time
    let character = flip fmap time $ \t ->
            let ix = floor $ nFrames * snd (properFraction (2 * (t - t0)))
            in  rsPlayer res ! ix
        pos = liftA2 (\(xOrig, yOrig) aspect ->
                (-xOrig - 800 * aspect, -yOrig)
            ) orig aspect
    return $ liftA3 (\draw orig pos ->
        draw (orig `plus` pos,(levelScale,levelScale))
      ) character orig pos
  where
    nFrames = realToFrac . succ . snd . A.bounds . rsPlayer $ res

game :: Platform p =>
        Resources p
     -> StdGen
     -> GameInput p
     -> Reactive (GameOutput p)
game res rng gi =
    pager (titlePage res rng) gi

