module PookieBathtime (pookieBathtime) where

import FRP.Sodium.GameEngine2D.Platform
import System.Random


data Resources p = Resources {
        pgPookieTitle :: Drawable p,
        pgPlayButton  :: Drawable p
    }

pookieBathtime :: Platform p => IO (GameInput p -> Reactive (GameOutput p))
pookieBathtime bgFiles = game <$> loadResources <*> newStdGen

loadResources :: Platform p => IO (Resources p)
loadResources = Resources
    <$> image "pookie-title.png"
    <*> image "play-button.png"

titlePage :: Platform p =>
             Resources p
          -> StdGen
          -> GameInput p
          -> Reactive (GameOutput p, Event (Page p))
titlePage res rng0 gi = do
    return (
        def {
            goSprite 
        }

game :: Platform p =>
        Resources p
     -> StdGen
     -> GameInput p
     -> Reactive (GameOutput p)
game res rng gi = pager (titlePage res rng)
