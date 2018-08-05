module Vivid.Jbb.Distrib.Museq where

import Control.Lens ((^.),(.~),(%~))
import Control.Monad.ST
import qualified Data.Vector as V
import Data.Vector.Algorithms.Intro (sortBy)
import Data.Vector.Algorithms.Search
  (binarySearchLBy, binarySearchRBy, Comparison)

import Vivid
import Vivid.Jbb.Distrib.Types


sortMuseq :: Museq -> Museq
sortMuseq = vec %~
  \v -> runST $ do v' <- V.thaw v
                   let compare' ve ve' = compare (fst ve) (fst ve')
                   sortBy compare' v'
                   V.freeze v'

-- | A valid Museq is sorted on start time, has (relative) duration > 0,
-- has its first action at time 0 and all actions at time < 1.
museqIsValid :: Museq -> Bool
museqIsValid mu = a && b && c && d where
  a = if V.length (_vec mu) == 0 then True
      else fst (V.head $ _vec mu) == 0
  b = if V.length (_vec mu) == 0 then True
      else fst (V.last $ _vec mu) < 1
  c = mu == sortMuseq mu
  d = _dur mu > 0

-- | time0 is the first time that had phase 0
nextPhase0 :: RealFrac a => a -> a -> a -> a
nextPhase0 time0 period now =
  fromIntegral (ceiling $ (now - time0) / period ) * period + time0

-- | PITFALL ? if lastPhase0 or nextPhase0 was called precisely at phase0,
-- both would yield the same result. Since time is measured in microseconds
-- there is exactly a one in a million chance of that.
prevPhase0 :: RealFrac a => a -> a -> a -> a
prevPhase0 time0 period now =
  fromIntegral (floor $ (now - time0) / period ) * period + time0

-- | >>> work in progress

--findNextEvents :: Time -> Duration -> Time -> RelDuration
--               -> Museq -> (Duration, [Action])
--findNextEvents time0 globalPeriod now museqPeriod museq =
--  let pp0 = lastPhase0 time0 (period * museqPeriod) now


-- | Finds the first (0-indexed) element of `v` which is >= to `a`.
-- If none such, returns length of vector.
-- When comparing Museq elements, a good comparison function is
-- to consider the Time and ignore the Action:
-- compare' ve ve' = compare (fst ve) (fst ve')
firstIndexGTE :: Comparison a -> V.Vector a -> a -> Int
firstIndexGTE comp v a = runST $ do
  v' <- V.thaw v
  return =<< binarySearchLBy comp v' a

-- | Finds the first (0-indexed) element of `v` which is > to `a`.
-- If none such, returns length of vector.
lastIndexGTE :: Comparison a -> V.Vector a -> a -> Int
lastIndexGTE comp v a = runST $ do
  v' <- V.thaw v
  return =<< binarySearchRBy comp v' a
