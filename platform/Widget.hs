{-# LANGUAGE RecursiveDo #-}
-- | Widget and layout
module Widget (
        Placement,
        AxisPlacement,
        Widget(..),
        widget,
        flow,
        Flow(..),
        reify,
        atStart,
        atCentre,
        atEnd,
        fill,
        expandable,
        (##),
        placement,
        pad
    ) where

import FRP.Sodium
import FRP.Sodium.GameEngine2D.Geometry
import Control.Applicative
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Traversable (sequenceA)
import Lens.Family
import Debug.Trace


-- | Parent's desired rectangle -> child desired sizes ->
--     (
--       (child-forced width, child-forced height),
--       aggregate child desired size,
--       (
--           sub-widget parent/bounding/outer rects,
--           sub-widget widget rects
--       ),
--       unused child desired sizes
--     )
newtype Placement = Placement (Rect -> [Vector] ->
    ((Bool, Bool), Vector, [(Rect, Rect)], [Vector]))

data Widget i o a = Widget {
        wiPlacement :: [Placement],  -- ^ Widget's desired size to actual rectangle
        -- | Desired size must not depend on input rect
        wiReify ::
               Behavior [(Rect, Rect)]  -- ^ List of (parent/outer/bounding, widget_ rectangles -
                                        -- reify must peel off what it needs to use
            -> i
               -- Output value, sprite, sound, desired size, parent rect remainder, widget rect remainder
            -> Reactive (a, o, [Behavior Vector], Behavior [(Rect,Rect)])
    }

-- | Widget receives the parent/outer/bounding rect and then the widget rect.
widget' :: Placement
        -> (Behavior Rect -> Behavior Rect -> i -> Reactive (a, o, Behavior Vector))
        -> Widget i o a
widget' placement f = Widget [placement] $ \rects i -> do
    let rect = fromMaybe (error $ "Widget.widget rects truncated!") . listToMaybe <$> rects
        rects' = tail <$> rects
    (a, o, sz) <- f (fst <$> rect) (snd <$> rect) i
    return (a, o, [sz], rects')

-- | Simple widget that just receives the widget rect.
widget :: Placement
       -> (Behavior Rect -> i -> Reactive (a, o, Behavior Vector))
       -> Widget i o a
widget placement f = widget' placement $ \_ rect i -> f rect i

instance Functor (Widget i o) where
    fmap f (Widget p r) = Widget p (\placement i -> fmap f' (r placement i))
      where f' (a, o, sz, placement) = (f a, o, sz, placement)

instance Monoid o => Applicative (Widget i o) where
    pure a = Widget {
            wiPlacement = [],
            wiReify = \rects _ -> return (a, mempty, [], rects)
        }
    wf <*> wa = Widget {
            wiPlacement = wiPlacement wf <> wiPlacement wa,
            wiReify = \rects i -> do
                (f, oF, szF, rects')  <- wiReify wf rects  i
                (a, oA, szA, rects'') <- wiReify wa rects' i
                return (f a, oF <> oA, szF <> szA, rects'') 
        }

instance (Monoid a, Monoid o) => Monoid (Widget i o a) where
    mempty = Widget {
            wiPlacement = [],
            wiReify = \rects _ -> return (mempty, mempty, [], rects)
        }
    wa `mappend` wb = Widget {
            wiPlacement = wiPlacement wa <> wiPlacement wb,
            wiReify = \rects i -> do
                (a, oA, szA, rects')  <- wiReify wa rects  i
                (b, oB, szB, rects'') <- wiReify wb rects' i
                return (a <> b, oA <> oB, szA <> szB, rects'')
        }

data Flow = Horizontal | Vertical

flow :: Flow -> Widget i o a -> Widget i o a
flow fl wi = Widget {
    wiPlacement = [Placement $ \p szs ->
        case wiPlacement wi of
            [] -> ((False, False),(0,0),[],szs)
            placements ->
                let split = case fl of
                                Horizontal -> splitLeft
                                Vertical   -> splitTop
                    whichAxis :: (a, a) -> a
                    whichAxis = case fl of
                                Horizontal -> fst
                                Vertical   -> snd
                    pLength = case fl of
                                Horizontal -> rectWidth p
                                Vertical   -> rectHeight p
                    place szs [] _ = []
                    place szs ((Placement f):placements') p =
                        let (childForced, childDesired, wrect, szs') = f prect szs
                            forced  = whichAxis childForced
                            desired = whichAxis childDesired
                            toChop = if forced then desired else desired+paddingAllocation
                            (prect, p') = split toChop p
                        in  ((forced, childDesired, prect, wrect), szs'):place szs' placements' p'
                    pls_szs = place szs placements p
                    (pls, szs's) = unzip pls_szs
                    szs' = last szs's
                    (childForceds, childDesiredsXY, prects, wrectss) = unzip4 pls
                    wrects = concat wrectss
                    childDesireds = map whichAxis childDesiredsXY
                    -- How many widgets sizes are parent-determined?
                    nParentDet = length (filter not childForceds)
                    -- Total of child-determined sizes
                    childForcedTotal = sum' childDesireds
                    -- Note: Not evaluated if n == 0, so no /0 error.
                    paddingAllocation = max 0 $ (pLength - childForcedTotal) / fromIntegral nParentDet
                    aggregateDesiredSz =
                        let (xs, ys) = unzip childDesiredsXY
                        in  case fl of
                                Horizontal -> (sum' xs, maximum0' ys)
                                Vertical   -> (maximum0' xs, sum' ys)
                    boundingWRect = foldl1' appendRect (map snd wrects)
                    boundingPRect = foldl1' appendRect (boundingWRect:prects) 
                in  ((True, True), aggregateDesiredSz, (boundingPRect, boundingWRect) : wrects, szs')
    ],
    wiReify = \rects i -> do
        -- Throw out the first item, which is the combined rectangles.
        -- These can be useful for functions that override this functionality,
        -- such as 'backdrop'.
        wiReify wi (tail <$> rects) i
  }
  where
    sum' = foldl' (+) 0
    maximum0' = foldl' max 0

reify :: Behavior Rect               -- ^ Desired rect
      -> i
      -> Widget i o a
      -> Reactive (a, o)
reify rect eMouse wi = do
    rec
        let place bszs (Placement f) =
                let bOut = f <$> rect <*> bszs
                in  (
                        fmap (\(_, _, _, szs') -> szs') bOut,
                        fmap (\(_, _, rects, _) -> rects) bOut
                    )
            (_, rects) = mapAccumL place (sequenceA szs) (wiPlacement wi)
        (out, o, szs, _) <- wiReify wi (concat <$> sequenceA rects) eMouse
    return (out, o)

-- | Parent's desired (origin, size) -> Widget desired size ->
--     (child forced size, final widget (origin, size))  
newtype AxisPlacement = AxisPlacement ((Coord, Coord) -> Coord -> (Bool, (Coord, Coord)))

-- | In centre of parent's space
atCentre :: AxisPlacement
atCentre = AxisPlacement $ \(po, pv) sz0 ->
    let sz = sz0*0.5
        v = max pv sz
    in  (True, (po, sz))

-- | At start of parent's space
atStart :: AxisPlacement
atStart = AxisPlacement $ \(po, pv) sz0 ->
    let sz = sz0*0.5
        v = max pv sz
    in  (True, (po - (v - sz), sz))

-- | At end of parent's space
atEnd :: AxisPlacement
atEnd = AxisPlacement $ \(po, pv) sz0 ->
    let sz = sz0*0.5
        v = max pv sz
    in  (True, (po + (v - sz), sz))

-- | Expand to fill parent's space
fill :: AxisPlacement
fill = AxisPlacement $ \(po, pv) sz0 ->
    let sz = sz0*0.5
        v = max pv sz
    in  (True, (po, v))

-- | Allow the widget to expand to area determined by parent
expandable :: AxisPlacement -> AxisPlacement
expandable (AxisPlacement f) = AxisPlacement $ \p cv -> (False, snd $ f p cv)

infixr 9 ##

-- | Combine X and Y placement into a 2D placement
(##) :: AxisPlacement  -- ^ Horizontal placement
     -> AxisPlacement  -- ^ Vertical placement
     -> Placement
AxisPlacement plx ## AxisPlacement ply = Placement $ \p@((pox, poy), (pvx, pvy)) ((cvx, cvy):cvs) ->
    -- We negate the Ys because X is increasing (left to right) but Y is decreasing (top to bottom).
    let (whichAxisx, (wox, wvx)) = plx (pox, pvx) cvx
        (whichAxisy, (woy, wvy)) = ply (-poy, pvy) cvy
    in  (
            (whichAxisx,whichAxisy),
            (cvx, cvy),
            [(p, ((wox,-woy),(wvx,wvy)))],
            cvs
        )

-- | Lens for placement
placement :: Functor f => (Placement -> f Placement) -> Widget i o a -> f (Widget i o a)
placement f (Widget [p] r) = fmap (\p' -> Widget [p'] r) (f p)
placement _ _ = error "Widget.placement can't use on null widget or combined widgets"

-- | Pad with the specified size horizontally and vertically
pad :: String -> (Coord, Coord) -> Placement -> Placement
pad descr (w, h) (Placement f) = Placement $ \p szs ->
    let (childForced, childDesired, wrect, szs') = f p szs
        {-(szw, szh):szs'
        szw' = szw + w
        szh' = szh + h -}
    in  trace("szs'="++show szs'++" "++descr) $ (childForced, childDesired, wrect, {- (seq szw' szw', seq szh' szh'): -} szs') 

{- TESTS -}

newtype Output = Output (Behavior [Rect])
instance Monoid Output where
    mempty = Output $ pure []
    mappend (Output a) (Output b) = Output $ liftA2 (++) a b

block :: Vector -> Widget () Output ()
block sz = widget (fill ## fill) $ \rect () -> do
    return ((), Output $ (:[]) <$> rect, pure sz)

main :: IO ()
main = do
    let {-wi = flow Vertical $
               (block (40, 20) & placement .~ atEnd ## fill)
            <* (block (40, 30) & placement .~ fill ## expandable atEnd) -}
        wi = flow Vertical $ mconcat $ replicate 5 (
               block (10, 10)
            )
    (rect, sendRect) <- sync $ newBehavior $ edgesRect (0,20,50,100)
    ((), Output rects) <- sync $ reify rect () wi
    kill <- sync $ listen (value rects) $ \rects -> print $ map rectEdges rects
    sync $ sendRect $ edgesRect (0,0,50,100)
    sync $ sendRect $ edgesRect (0,90,50,100)
    sync $ sendRect $ edgesRect (10,90,20,100)
    kill

