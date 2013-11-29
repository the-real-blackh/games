module UIWidget (
        UIWidget,
        UIOutput(..),
        backdrop,
        module Widget
    ) where

import Widget
import FRP.Sodium
import FRP.Sodium.GameEngine2D.Platform
import Control.Applicative
import Data.Maybe
import Data.Monoid
import Debug.Trace


data UIOutput p = UIOutput (Behavior (Sprite p)) (Event (Sound p))

instance Platform p => Monoid (UIOutput p) where
    mempty = UIOutput (pure mempty) never
    UIOutput ba sa `mappend` UIOutput bb sb = UIOutput (liftA2 mappend ba bb) (sa <> sb)

type UIWidget p = Widget (Event (MouseEvent p)) (UIOutput p)

backdrop :: Platform p =>
            Drawable p -> UIWidget p a -> UIWidget p a
backdrop draw wi = wi {
        wiReify = \rects eMouse -> do
            let prect = fst . fromMaybe (error $ "Widget.widget rects truncated!") . listToMaybe <$> rects
            (a, UIOutput spr snd, szs, rects) <- wiReify wi rects eMouse
            let spr' = liftA2 mappend (draw <$> prect) spr
            return (a, UIOutput spr' snd, szs, rects)
    }

