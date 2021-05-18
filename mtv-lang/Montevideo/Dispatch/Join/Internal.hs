{-# LANGUAGE ScopedTypeVariables, ViewPatterns, TupleSections #-}

module Montevideo.Dispatch.Join.Internal (
    timeForBoth_toAppearToFinish      -- ^ Museq l a -> Museq m b -> RTime
  , timeForBoth_toFinish -- ^ Museq l a -> Museq m b -> RTime
  , explicitReps -- ^ forall a. Museq l a -> [V.Vector (Ev l a)]
  , unsafeExplicitReps -- ^ forall l a.
     -- RTime -> Museq l a -> [V.Vector (Ev l a)]
  , boundaries -- ^ forall a. Real a => [(a,a)] -> [a]
  , partitionAndGroupEventsAtBoundaries -- ^ forall a l t v. Real t
     -- => [t] -> [Event t l a] -> [Event t l a]
  , partitionArcAtTimes -- ^ Real a => [a] -> (a,a) -> [(a,a)]
  , alignAndJoin,joinAlignedEvents -- ^ forall a b c t. Real t
     --                       => (a -> b -> c)
     --                       -> [Event t String a]
     --                       -> [Event t String b]
     --                       -> [Event t String c]
  ) where

import Control.Lens hiding (op,indexed)
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Vector as V

import Montevideo.Dispatch.Time
import Montevideo.Dispatch.Types
import Montevideo.Util


timeForBoth_toAppearToFinish :: Museq l a -> Museq m b -> RTime
timeForBoth_toAppearToFinish x y =
  RTime $ lcmRatios (tr $ timeToAppearToFinish x) (tr $ timeToAppearToFinish y)

timeForBoth_toFinish :: Museq l a -> Museq m b -> RTime
timeForBoth_toFinish x y =
  RTime $ lcmRatios (tr $ timeToFinish x) (tr $ timeToFinish y)

-- | If L is the length of time such that `m` finishes at phase 0,
-- divide the events of L every multiple of _dur.
-- See the test suite for an example.
explicitReps :: forall a l. Museq l a -> [V.Vector (Ev l a)]
explicitReps m = unsafeExplicitReps (timeToFinish m) m

-- | PITFALL: I don't know what this will do if
-- `totalDuration` is not an integer multiple of `timeToFinish m`
unsafeExplicitReps :: forall l a.
  RTime -> Museq l a -> [V.Vector (Ev l a)]
unsafeExplicitReps totalDuration m = reps where
  sups = round $ totalDuration / _sup m
    -- It takes a duration equal to this many multiples of _sup m
    -- for m to finish at phase 0.
    -- It's already an integer; `round` is just to prove that to GHC.
  durs :: Int = round $ totalDuration / _dur m
  indexed = zip [0..sups-1]
    $ repeat $ _vec m :: [(Int,V.Vector (Ev l a))]
  adjustTimes :: (Int,V.Vector (Ev l a)) -> V.Vector (Ev l a)
  adjustTimes (idx,v) = V.map f v where
    f = over evArc (\(x,y) -> (g x, g y)) where
      g = (+) $ fromIntegral idx * _sup m
  spread = V.concat $ map adjustTimes indexed :: V.Vector (Ev l a)
    -- the times in `spread` range from 0 to `timeToAppearToFinish m`
  maixima = [fromIntegral i * _dur m | i <- [1..durs]]
  reps = divideAtMaxima (view evStart) maixima spread :: [V.Vector (Ev l a)]


-- | = Merge-ish (`merge`, `<*>` and `meta`) functions.

-- | Produces a sorted list of arc endpoints,
-- such that the start and the end of every event is among those endpoints.
-- The tests make it clear.
-- If `arcs` includes `(x,x)`, then `x` will appear twice in the output;
-- otherwise the arc `(x,x)` would not be constructible from the endpoints.
boundaries :: forall a. Real a => [(a,a)] -> [a]
boundaries arcs0 = doubleTheDurationZeroBoundaries arcs0
                  $ L.sort $ unique $ map fst arcs0 ++ map snd arcs0 where
  doubleTheDurationZeroBoundaries :: [(a,a)] -> [a] -> [a]
  doubleTheDurationZeroBoundaries arcs bounds = concatMap f bounds where
    instants :: S.Set a
    instants = S.fromList $ map fst $ filter (\(s,e) -> s == e) arcs
    f t = if S.member t instants then [t,t] else [t]

-- | Divides long events at any intermediate boundaries from other events.
-- Then sorts those events.
-- ASSUMES the first input includes each value in the second.
-- (If the first list comes from `boundaries`, it will include those.)
partitionAndGroupEventsAtBoundaries :: forall a l t. Real t
  => [t] -> [Event t l a] -> [Event t l a]
partitionAndGroupEventsAtBoundaries bs evs =
  let partitionEv :: Event t l a -> [Event t l a]
      partitionEv ev = map rebuild $ partitionArcAtTimes bs $ _evArc ev
        where rebuild someArc = ev {_evArc = someArc}
  in L.sortOn _evArc $ concatMap partitionEv evs

-- | ASSUMES list of times is sorted, uniqe, and includes endpoints of `arc`.
-- (If the times come from `boundaries`, those endpoints are included.)
partitionArcAtTimes :: Real a
                    => [a]   -- ^ the times to partition at
                    -> (a,a) -- ^ the arc to partition
                    -> [(a,a)]
partitionArcAtTimes (a:b:ts) (c,d)
  | c > a = partitionArcAtTimes (b:ts) (c,d)
  | b == d = [(a,b)]
  | otherwise = (a,b) : partitionArcAtTimes (b:ts) (b,d)
partitionArcAtTimes _ _ =
  error "partitionArcAtTimes: uncaught input pattern."

-- | = PITFALL: `alignAndJoin`  and `joinAlignedEvents`
-- are mutually recursive.
--
-- Events are `aligned` iff they start and end at the same time.
-- To `join` two events is not defined here;
-- it just means applying some user-provided operator to them.
--
-- `alignAndJoin` checks whether events are aligned.
-- If so, it hands work off to `joinAlignedEvents`.
-- If not, it discards data until they are.
--
-- `joinAlignedEvents` finds events in the second input aligned with first event
-- in first input, joins them, passes remaining work back to `alignAndJoin`.
--
-- In the `Museq` version, Event labels from an output event's
-- two inputs are concatenated, which prevents interference.
-- TODO ? A version that permits interference.
alignAndJoin,joinAlignedEvents :: forall a b c t. Real t
  => (a -> b -> c) -- ^ how to join them
  -> [Event t String a]
  -> [Event t String b]
  -> [Event t String c]

alignAndJoin _ [] _ = []
alignAndJoin _ _ [] = []
alignAndJoin op as bs
  | _evArc (head as) <   _evArc (head bs) = alignAndJoin op (tail as) bs
  | _evArc (head as) >   _evArc (head bs) = alignAndJoin op as (tail bs)
  | _evArc (head as) ==  _evArc (head bs) = joinAlignedEvents op as bs
alignAndJoin _ _ _ = error "alignAndJoin: uncaught input pattern."

joinAlignedEvents op (a:as) bs =
  joined ++ alignAndJoin op as bs where
  bsMatch = takeWhile ((== _evArc a) . _evArc) bs
  joined = over evData (op $ _evData a)
         . over evLabel -- concatenate event labels
           (deleteShowQuotes . ((++) $ _evLabel a))
         <$> bsMatch
joinAlignedEvents _ _ _ =
  error "joinAlignedEvents: uncaught input pattern."
