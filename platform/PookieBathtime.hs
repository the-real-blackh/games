module PookieBathtime (pookieBathtime) where

import Page

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
import Data.Monoid
import Data.Traversable
import System.Random


data Resources p = Resources {
        rsGameBG       :: Sprite p,
        rsPookieTitle  :: Sprite p,
        rsPlayButton   :: Drawable p,
        rsPlayer       :: Array Int (Drawable p),
        rsWomanWithDog :: Sprite p
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
    let eEnd = filterJust $ fmap (\t -> if t - t0 >= 3 then Just $ gamePage res rng0 else Nothing)
                   (updates $ giTime gi)
    return (
        def {
            goSprite = pure $ rsWomanWithDog res
        },
        eEnd
      )

gamePage :: Platform p =>
            Resources p
         -> StdGen
         -> Page p
gamePage res rng0 = Page $ \gi -> do
    plSpr <- player res (giTime gi) never
    return (
        def {
            goSprite = mconcat <$> sequenceA [
                pure $ rsGameBG res,
                plSpr
              ]
        },
        never
      )

player :: Platform p =>
          Resources p
       -> Behavior Double
       -> Event ()            -- ^ Jump
       -> Reactive (Behavior (Sprite p))
player res time eJump = do
    t0 <- sample time
    let character = flip fmap time $ \t ->
            let ix = floor $ nFrames * snd (properFraction (2 * (t - t0)))
            in  rsPlayer res ! ix
    return $ ($ ((0,0),(200,200))) <$> character
  where
    nFrames = realToFrac . succ . snd . A.bounds . rsPlayer $ res

game :: Platform p =>
        Resources p
     -> StdGen
     -> GameInput p
     -> Reactive (GameOutput p)
game res rng gi =
    pager (titlePage res rng) gi

