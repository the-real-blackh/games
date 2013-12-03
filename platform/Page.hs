{-# LANGUAGE RecursiveDo #-}
module Page where

import Control.Applicative
import FRP.Sodium
import FRP.Sodium.GameEngine2D.Platform

data Page p = Page {
        pgReactive :: GameInput p -> Reactive (GameOutput p, Event (Page p))
    }

pager :: Platform p =>
         Page p -> GameInput p -> Reactive (GameOutput p)
pager pg0 gi = do
    po0 <- pgReactive pg0 gi
    rec
        po <- hold po0 . execute . fmap (\pg -> pgReactive pg gi) $ ePage
        let ePage = switchE (snd <$> po)
    let go = fst <$> po
    GameOutput <$> switch (goSprite <$> go)
               <*> switch (goMusic <$> go)
               <*> pure (switchE (goEffects <$> go))
