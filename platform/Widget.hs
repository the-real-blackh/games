{-# LANGUAGE RecursiveDo #-}
-- | Widget and layout
module Widget where

import FRP.Sodium
import FRP.Sodium.GameEngine2D.Geometry
import Control.Applicative
import Data.List
import Data.Maybe
import Data.Monoid
import Data.Traversable (sequenceA)


-- | Parent's desired rectangle -> child desired sizes ->
--     ((child-forced width, child-forced height), aggregate child desired size, widget rect, unused child desired sizes)
newtype Placement = Placement (Rect -> [Vector] ->
    ((Bool, Bool), Vector, [Rect], [Vector]))

data Widget i o a = Widget {
        wiPlacement :: [Placement],  -- ^ Widget's desired size to actual rectangle
        -- | Desired size must not depend on input rect
        wiReify ::
               Behavior [Rect]
            -> i
               -- Output value, sprite, sound, desired size
            -> Reactive (a, o, [Behavior Vector], Behavior [Rect]) 
    }

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
                (f, oA, szF, rects')  <- wiReify wf rects  i
                (a, oB, szA, rects'') <- wiReify wa rects' i
                return (f a, oA <> oB, szF <> szA, rects'') 
        }

data Flow = Horizontal | Vertical

flow :: Flow -> Widget i o a -> Widget i o a
flow fl wi = wi {
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
                        let (childForced, childDesired, wrect, szs') = f rect szs
                            forced  = whichAxis childForced
                            desired = whichAxis childDesired
                            sz = if forced then desired else desired+paddingAllocation
                            (rect, p') = split sz p
                        in  ((forced, childDesired, wrect), szs'):place szs' placements' p'
                    pls_szs = place szs placements p
                    (pls, szs's) = unzip pls_szs
                    szs' = last szs's
                    (childForceds, childDesiredsXY, wrects) = unzip3 pls :: ([Bool], [Vector], [[Rect]])
                    childDesireds = map whichAxis childDesiredsXY :: [Coord]
                    -- How many widgets sizes are parent-determined?
                    nParentDet = length (filter id childForceds)
                    -- Total of child-determined sizes
                    childForcedTotal = sum' childDesireds
                    -- Note: Not evaluated if n == 0, so no /0 error.
                    paddingAllocation = max 0 $ (pLength - childForcedTotal) / fromIntegral nParentDet
                    aggregateDesiredSz =
                        let (xs, ys) = unzip childDesiredsXY
                        in  case fl of
                                Horizontal -> (sum' xs, maximum0' ys)
                                Vertical   -> (maximum0' xs, sum' ys)
                in  ((True, True), aggregateDesiredSz, concat wrects, szs')
    ]
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
nullz :: AxisPlacement
nullz = AxisPlacement $ \(po, pv) sz0 ->
    (True, (10,10))

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

{-
expandX :: Widget i o a -> Widget i o a
expandX wi = wi {
        wiPlacement = flip map (wiPlacement wi) $ \(Placement f) -> Placement $
            \rect szs -> let ((_, forcedY), desired, wrect, szs') = f rect szs
                         in  ((True, forcedY), desired, wrect, szs') 
    }

expandY :: Widget i o a -> Widget i o a
expandY wi = wi {
        wiPlacement = flip map (wiPlacement wi) $ \(Placement f) -> Placement $
            \rect szs -> let ((forcedX, _), desired, wrect, szs') = f rect szs
                         in  ((forcedX, True), desired, wrect, szs') 
    }
    -}

infixr 9 ##

-- | Combine X and Y placement into a 2D placement
(##) :: AxisPlacement  -- ^ Horizontal placement
     -> AxisPlacement  -- ^ Vertical placement
     -> Placement
AxisPlacement plx ## AxisPlacement ply = Placement $ \((pox, poy), (pvx, pvy)) ((cvx, cvy):cvs) ->
    -- We negate the Ys because X is increasing (left to right) but Y is decreasing (top to bottom).
    let (whichAxisx, (wox, wvx)) = plx (pox, pvx) cvx
        (whichAxisy, (woy, wvy)) = ply (-poy, pvy) cvy
    in  (
            (whichAxisx,whichAxisy),
            (cvx, cvy),
            [((wox,-woy),(wvx,wvy))],
            cvs
        )

{-
newtype Output = Output (Behavior [Rect])
instance Monoid Output where
    mempty = Output $ pure []
    mappend (Output a) (Output b) = Output $ liftA2 (++) a b

block :: Vector -> Widget () Output ()
block sz = Widget [fill ## fill] $ \rects () -> do
    let rect = head <$> rects
        rects' = tail <$> rects
    return ((), Output $ (:[]) <$> rect, [pure sz], rects')

main :: IO ()
main = do
    let wi = flow Vertical $
               (block (40, 20)) { wiPlacement = [atEnd ## fill] }
            <* (block (40, 30)) { wiPlacement = [fill ## expandable atEnd] }
    (rect, sendRect) <- sync $ newBehavior $ edgesRect (0,20,50,100)
    ((), Output rects) <- sync $ reify rect () wi
    kill <- sync $ listen (value rects) $ \rects -> print $ map rectEdges rects
    sync $ sendRect $ edgesRect (0,0,50,100)
    sync $ sendRect $ edgesRect (0,90,50,100)
    kill
-}

