module Vivid.Jbb.Distrib.Museq where

import Control.Lens ((^.),(.~),(%~))
import Control.Monad.ST
import qualified Data.Vector as V
import           Data.Vector ((!))
import Data.Vector.Algorithms.Intro (sortBy)

import Vivid
import Vivid.Jbb.Distrib.Types
import Vivid.Jbb.Util
import Vivid.Jbb.Synths (SynthDefEnum(Boop))


sortMuseq :: Museq a -> Museq a
sortMuseq = vec %~
  \v -> runST $ do v' <- V.thaw v
                   let compare' ve ve' = compare (fst ve) (fst ve')
                   sortBy compare' v'
                   V.freeze v'

-- | A valid Museq is sorted on start time, has (relative) duration > 0,
-- and all actions at time < 1.
museqIsValid :: Eq a => Museq a -> Bool
museqIsValid mu = b && c && d where
  b = if V.length (_vec mu) == 0 then True
      else fst (V.last $ _vec mu) < 1
  c = mu == sortMuseq mu
  d = _dur mu > 0

-- todo ? This could be made a little faster by using binarySearchRByBounds
-- instead of binarySearchR, to avoid searching the first part
-- of the vector again.
-- | Returns a list of actions and the time remaining until they start.
findNextEvents :: Time -> Duration -> Time
               -> Museq Action -> (Duration, [Action])
findNextEvents time0 globalPeriod now museq =
  let period = globalPeriod * fromRational (_dur museq)
      pp0 = prevPhase0 time0 period now
      relNow = toRational $ (now - pp0) / period
      vecLen = V.length $ _vec museq
      compare' :: (RelDuration, a) -> (RelDuration, a) -> Ordering
      compare' ve ve' = compare (fst ve) (fst ve')
      dummyAction = New Boop "marge"
      startOrOOB = firstIndexGTE  compare' (_vec museq) (relNow, dummyAction)
      start = if startOrOOB < vecLen then startOrOOB else 0
      end = lastIndexJustGTE compare' (_vec museq) (_vec museq ! start)
      relTimeOfNextEvent = if startOrOOB == start
                           then        fst $ _vec museq ! start
                           else (+1) $ fst $ _vec museq ! 0
      timeUntilNextEvent =
        fromRational relTimeOfNextEvent * period + pp0 - now
  in ( timeUntilNextEvent
     , map snd $ V.toList $ V.slice start (end - start) $ _vec museq )
