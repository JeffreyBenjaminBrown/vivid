{-# LANGUAGE ScopedTypeVariables, ViewPatterns #-}

module Vivid.Jbb.Dispatch.Internal.Join
where

import Control.Lens (over, _1)
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Vector as V

import Vivid.Jbb.Util
import Vivid.Jbb.Dispatch.Museq
import Vivid.Jbb.Dispatch.Types


timeForBothToRepeat :: Museq a -> Museq a -> RTime
timeForBothToRepeat x y =
  RTime $ lcmRatios (tr $ timeToRepeat x) (tr $ timeToRepeat y)

-- | if L is the length of time such that `m` finishes at phase 0,
-- divide the events of L every multiple of _dur.
-- See the test suite for an example.
explicitReps :: forall a. Museq a -> [V.Vector ((RTime,RTime),a)]
explicitReps m = unsafeExplicitReps (timeToPlayThrough m) m

-- | PITFALL: I don't know what this will do if
-- `totalDuration` is not an integer multiple of `timeToPlayThrough m`
unsafeExplicitReps :: forall a.
  RTime -> Museq a -> [V.Vector ((RTime,RTime),a)]
unsafeExplicitReps totalDuration m =
  let sups = round $ totalDuration / _sup m
        -- It takes a duration equal to this many multiples of _sup m
        -- for m to finish at phase 0.
        -- It's already an integer; `round` is just to prove that to GHC.
      durs = round $ totalDuration / _dur m
      indexed = zip [0..sups-1]
        $ repeat $ _vec m :: [(Int,V.Vector ((RTime,RTime),a))]
      adjustTimes :: (Int,V.Vector ((RTime,RTime),a))
                  ->      V.Vector ((RTime,RTime),a)
      adjustTimes (idx,v) = V.map f v where
        f = over _1 (\(x,y) -> (f x, f y)) where
          f = (+) $ fromIntegral idx * _sup m
      spread = V.concat $ map adjustTimes indexed
        :: V.Vector ((RTime,RTime),a)
        -- the times in `spread` range from 0 to `timeToRepeat m`
      maixima = [fromIntegral i * _dur m | i <- [1..durs]]
      reps = divideAtMaxima (fst . fst) maixima spread
        :: [V.Vector ((RTime,RTime),a)]
  in reps

-- | = Merge-related functions.

-- | Produces a sorted list of arc endpoints.
-- If `arcs` includes `(x,x)`, then `x` will appear twice in the output.
boundaries :: forall a. Real a => [(a,a)] -> [a]
boundaries arcs = doubleTheDurationZeroBoundaries arcs
                  $ L.sort $ unique $ map fst arcs ++ map snd arcs where
  doubleTheDurationZeroBoundaries :: [(a,a)] -> [a] -> [a]
  doubleTheDurationZeroBoundaries arcs bounds = concatMap f bounds where
    instants :: S.Set a
    instants = S.fromList $ map fst $ filter (\(s,e) -> s == e) arcs
    f t = if S.member t instants then [t,t] else [t]
