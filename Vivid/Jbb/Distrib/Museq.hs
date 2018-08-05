module Vivid.Jbb.Distrib.Museq where

import Control.Lens ((^.),(.~),(%~))
import Control.Monad.ST
import qualified Data.Vector as V
import Data.Vector.Algorithms.Intro (sortBy)
import Data.Vector.Mutable
import Data.Vector.Algorithms.Search (binarySearchLBy)

import Vivid
import Vivid.Jbb.Distrib.Types


sortMuseq :: Museq -> Museq
sortMuseq = vec %~
  \v -> runST $ do v' <- V.thaw v
                   let compare' ve ve' = compare (fst ve) (fst ve')
                   sortBy compare' v'
                   V.freeze v'

museqIsValid :: Museq -> Bool
museqIsValid mu = a && b && c && d where
  a = if V.length (_vec mu) == 0 then True
      else fst (V.head $ _vec mu) == 0
  b = if V.length (_vec mu) == 0 then True
      else fst (V.last $ _vec mu) < _dur mu
  c = mu == sortMuseq mu
  d = _dur mu > 0

-- | time0 is the first time that had phase 0
nextPhase0 :: RealFrac a => a -> a -> a -> a
nextPhase0 time0 period now =
  fromIntegral (ceiling $ (now - time0) / period ) * period + time0

-- | PITFALL ? if lastPhase0 or nextPhase0 was called precisely at phase0,
-- both would yield the same result. Since time is measured in microseconds
-- there is exactly a one in a million chance of that.
lastPhase0 :: RealFrac a => a -> a -> a -> a
lastPhase0 time0 period now =
  fromIntegral (floor $ (now - time0) / period ) * period + time0

-- | >>> work in progress

--findNextEvents :: Time -> Duration -> Time -> Museq -> (Duration, [Action])
--findNextEvents time0 period now museq =
--  let np0 = nextPhase0 time0 period now

searchTest :: V.Vector (Int,Int) -> (Int,Int) -> Int
searchTest v (a,b) = runST $ do
  v' <- V.thaw v
  let compare' ve ve' = compare (fst ve) (fst ve')
  return =<< binarySearchLBy compare' v' (a,b)
