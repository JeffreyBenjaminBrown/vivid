{-# LANGUAGE ScopedTypeVariables, ViewPatterns, TupleSections #-}

module Vivid.Jbb.Dispatch.Internal.Join (
    timeForBothToRepeat -- ^ Museq a -> Museq b -> RTime
  , timeForBothToRepeat' -- ^ Museq' l a -> Museq' m b -> RTime
  , explicitReps -- ^ forall a. Museq a -> [V.Vector (Ev a)]
  , unsafeExplicitReps -- ^ forall a.
     -- RTime -> Museq a -> [V.Vector (Ev a)]
  , unsafeExplicitReps' -- ^ forall l a.
     -- RTime -> Museq' l a -> [V.Vector (Ev' l a)]
  , boundaries -- ^ forall a. Real a => [(a,a)] -> [a]
  , partitionArcAtTimes -- ^ Real a => [a] -> (a,a) -> [(a,a)]
  , partitionAndGroupEventsAtBoundaries -- ^ forall a v. Real a
     -- => [a] -> [((a,a),v)] -> [((a,a),v)]
  , partitionAndGroupEventsAtBoundaries' -- ^ forall a l t v. Real t
     -- => [t] -> [Event t l a] -> [Event t l a]
  , alignAndJoin,joinEvents -- ^ forall a b c.
     -- (a -> b -> c) -> [Ev a] -> [Ev b] -> [Ev c]
  , alignAndJoin',joinEvents' -- ^ forall a b c t. Real t
     --                       => (a -> b -> c)
     --                       -> [Event t String a]
     --                       -> [Event t String b]
     --                       -> [Event t String c]
  ) where

import Control.Lens (over, view, _1, _2)
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Vector as V

import Vivid.Jbb.Util
import Vivid.Jbb.Dispatch.Museq
import Vivid.Jbb.Dispatch.Types


timeForBothToRepeat :: Museq a -> Museq b -> RTime
timeForBothToRepeat x y =
  RTime $ lcmRatios (tr $ timeToRepeat x) (tr $ timeToRepeat y)

timeForBothToRepeat' :: Museq' l a -> Museq' m b -> RTime
timeForBothToRepeat' x y =
  RTime $ lcmRatios (tr $ timeToRepeat' x) (tr $ timeToRepeat' y)

-- | if L is the length of time such that `m` finishes at phase 0,
-- divide the events of L every multiple of _dur.
-- See the test suite for an example.
explicitReps :: forall a. Museq a -> [V.Vector (Ev a)]
explicitReps m = unsafeExplicitReps (timeToPlayThrough m) m

-- | PITFALL: I don't know what this will do if
-- `totalDuration` is not an integer multiple of `timeToPlayThrough m`
unsafeExplicitReps :: forall a.
  RTime -> Museq a -> [V.Vector (Ev a)]
unsafeExplicitReps totalDuration m = reps where
  sups = round $ totalDuration / _sup m
    -- It takes a duration equal to this many multiples of _sup m
    -- for m to finish at phase 0.
    -- It's already an integer; `round` is just to prove that to GHC.
  durs :: Int = round $ totalDuration / _dur m
  indexed = zip [0..sups-1]
    $ repeat $ _vec m :: [(Int,V.Vector (Ev a))]
  adjustTimes :: (Int,V.Vector (Ev a)) -> V.Vector (Ev a)
  adjustTimes (idx,v) = V.map f v where
    f = over _1 (\(x,y) -> (g x, g y)) where
      g = (+) $ fromIntegral idx * _sup m
  spread = V.concat $ map adjustTimes indexed :: V.Vector (Ev a)
    -- the times in `spread` range from 0 to `timeToRepeat m`
  maixima = [fromIntegral i * _dur m | i <- [1..durs]]
  reps = divideAtMaxima (fst . fst) maixima spread :: [V.Vector (Ev a)]

unsafeExplicitReps' :: forall l a.
  RTime -> Museq' l a -> [V.Vector (Ev' l a)]
unsafeExplicitReps' totalDuration m = reps where
  sups = round $ totalDuration / _sup' m
    -- It takes a duration equal to this many multiples of _sup m
    -- for m to finish at phase 0.
    -- It's already an integer; `round` is just to prove that to GHC.
  durs :: Int = round $ totalDuration / _dur' m
  indexed = zip [0..sups-1]
    $ repeat $ _vec' m :: [(Int,V.Vector (Ev' l a))]
  adjustTimes :: (Int,V.Vector (Ev' l a)) -> V.Vector (Ev' l a)
  adjustTimes (idx,v) = V.map f v where
    f = over evArc (\(x,y) -> (g x, g y)) where
      g = (+) $ fromIntegral idx * _sup' m
  spread = V.concat $ map adjustTimes indexed :: V.Vector (Ev' l a)
    -- the times in `spread` range from 0 to `timeToRepeat m`
  maixima = [fromIntegral i * _dur' m | i <- [1..durs]]
  reps = divideAtMaxima (view evStart) maixima spread :: [V.Vector (Ev' l a)]

-- | = Merge-ish (`merge`, `<*>` and `meta`) functions.

-- | Produces a sorted list of arc endpoints.
-- If `arcs` includes `(x,x)`, then `x` will appear twice in the output.
boundaries :: forall a. Real a => [(a,a)] -> [a]
boundaries arcs0 = doubleTheDurationZeroBoundaries arcs0
                  $ L.sort $ unique $ map fst arcs0 ++ map snd arcs0 where
  doubleTheDurationZeroBoundaries :: [(a,a)] -> [a] -> [a]
  doubleTheDurationZeroBoundaries arcs bounds = concatMap f bounds where
    instants :: S.Set a
    instants = S.fromList $ map fst $ filter (\(s,e) -> s == e) arcs
    f t = if S.member t instants then [t,t] else [t]

-- | ASSUMES list of times is sorted, uniqe, and includes endpoints of `arc`.
-- (If the times come from `boundaries`, they will include those.)
partitionArcAtTimes :: Real a => [a] -> (a,a) -> [(a,a)]
partitionArcAtTimes (a:b:ts) (c,d)
  | c > a = partitionArcAtTimes (b:ts) (c,d)
  | b == d = [(a,b)]
  | otherwise = (a,b) : partitionArcAtTimes (b:ts) (b,d)
partitionArcAtTimes _ _ =
  error "partitionArcAtTimes: uncaught input pattern."

-- | ASSUMES the first input includes each value in the second.
-- (If the first list comes from `boundaries`, it will include those.)
partitionAndGroupEventsAtBoundaries :: forall a v. Real a
  => [a] -> [((a,a),v)] -> [((a,a),v)]
partitionAndGroupEventsAtBoundaries bs evs =
  let partitionEv :: ((a,a),v) -> [((a,a),v)]
      partitionEv (someArc,x) = map (,x) $
                                partitionArcAtTimes bs someArc
  in L.sortOn fst $ concatMap partitionEv evs

partitionAndGroupEventsAtBoundaries' :: forall a l t. Real t
  => [t] -> [Event t l a] -> [Event t l a]
partitionAndGroupEventsAtBoundaries' bs evs =
  -- TODO ? Use Traversal to simplify
  let partitionEv :: Event t l a -> [Event t l a]
      partitionEv ev = map rebuild $ partitionArcAtTimes bs $ _evArc ev
        where rebuild someArc = ev {_evArc = someArc}
  in L.sortOn _evArc $ concatMap partitionEv evs

-- | `alignAndJoin`  and `joinEvents` are mutually recursive.
-- `alignAndJoin` checks whether events are aligned.
-- If so, it hands work off to `joinEvents`.
-- `joinEvents` finds events in the second input aligned with first event
-- in first input, joins them, passes remaining work back to `joinEvents`.
--
-- In the Museq' version, Event labels from an output event's
-- two inputs are concatenated, which prevents interference.
-- TODO ? A version that permits interference.
alignAndJoin,joinEvents :: forall a b c.
  (a -> b -> c) -> [Ev a] -> [Ev b] -> [Ev c]

alignAndJoin _ [] _ = []
alignAndJoin _ _ [] = []
alignAndJoin op aEvs@((arcA,_):aRest)  bEvs@((arcB,_):bRest)
  | arcA <  arcB = alignAndJoin op aRest bEvs
  | arcB <  arcA = alignAndJoin op aEvs bRest
  | arcA == arcB = joinEvents op aEvs bEvs
alignAndJoin _ _ _ = error "alignAndJoin: uncaught input pattern."

joinEvents op ((arc0,a):aEvs) bEvs =
  joined ++ alignAndJoin op aEvs bEvs
  where bEvsMatch = takeWhile ((== arc0) . fst) bEvs
        joined = over _2 (op a) <$> bEvsMatch
joinEvents _ _ _ = error "joinEvents: uncaught input pattern."

alignAndJoin',joinEvents' :: forall a b c t. Real t
                          => (a -> b -> c)
                          -> [Event t String a]
                          -> [Event t String b]
                          -> [Event t String c]

alignAndJoin' _ [] _ = []
alignAndJoin' _ _ [] = []
alignAndJoin' op as bs
  | _evArc (head as) <   _evArc (head bs) = alignAndJoin' op (tail as) bs
  | _evArc (head as) >   _evArc (head bs) = alignAndJoin' op as (tail bs)
  | _evArc (head as) ==  _evArc (head bs) = joinEvents' op as bs
alignAndJoin' _ _ _ = error "alignAndJoin': uncaught input pattern."

joinEvents' op (a:as) bs = joined ++ alignAndJoin' op as bs where
  bsMatch = takeWhile ((== _evArc a) . _evArc) bs
  joined = over evData (op $ _evData a)
         . over evLabel -- concatenate event labels
           (deleteShowQuotes . ((++) $ _evLabel a))
         <$> bsMatch
joinEvents' _ _ _ = error "joinEvents': uncaught input pattern."
