module PookieBathtime (pookieBathtime) where

import Page

import FRP.Sodium
import FRP.Sodium.GameEngine2D.Geometry
import FRP.Sodium.GameEngine2D.Gesture
import FRP.Sodium.GameEngine2D.Platform

import Control.Applicative
import Control.Arrow (first)
import Data.Default
import Data.Monoid
import Data.Traversable
import System.Random


data Resources p = Resources {
        pgPookieTitle :: Sprite p,
        pgPlayButton  :: Drawable p,
        pgWomanWithDog :: Sprite p
    }

pookieBathtime :: Platform p => IO (GameInput p -> Reactive (GameOutput p))
pookieBathtime = game <$> loadResources <*> newStdGen

loadResources :: Platform p => IO (Resources p)
loadResources = Resources
    <$> backgroundImage "pookie-title.png"
    <*> image "play-button.png"
    <*> backgroundImage "woman-with-dog.jpg"

titlePage :: Platform p =>
             Resources p
          -> StdGen
          -> Page p
titlePage res rng0 = Page $ \gi -> do
    (eClick, held) <- clickGesture (pure (`inside` buttonRectUp)) (giMouse gi)
    return (
        def {
            goSprite = (pgPookieTitle res <>) . mconcat <$> sequenceA [
                   (\held -> pgPlayButton res (if held then buttonRectDown else buttonRectUp)) <$> held
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
    return (
        def {
            goSprite = pure $ pgWomanWithDog res
        },
        never
      )

game :: Platform p =>
        Resources p
     -> StdGen
     -> GameInput p
     -> Reactive (GameOutput p)
game res rng gi =
    pager (titlePage res rng) gi

