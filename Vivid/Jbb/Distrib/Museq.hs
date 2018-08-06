module Vivid.Jbb.Distrib.Museq where

import Control.Lens ((^.),(.~),(%~))
import Control.Monad.ST
import qualified Data.Vector as V
import           Data.Vector ((!))
import Data.Vector.Algorithms.Intro (sortBy)

import Vivid
import Vivid.Jbb.Distrib.Types
import Vivid.Jbb.Distrib.Util
import Vivid.Jbb.Synths (SynthDefEnum(Boop))


sortMuseq :: Museq -> Museq
sortMuseq = vec %~
  \v -> runST $ do v' <- V.thaw v
                   let compare' ve ve' = compare (fst ve) (fst ve')
                   sortBy compare' v'
                   V.freeze v'

-- | A valid Museq is sorted on start time, has (relative) duration > 0,
-- and all actions at time < 1.
museqIsValid :: Museq -> Bool
museqIsValid mu = b && c && d where
  b = if V.length (_vec mu) == 0 then True
      else fst (V.last $ _vec mu) < 1
  c = mu == sortMuseq mu
  d = _dur mu > 0

-- TODO ? This could be made a little faster by using binarySearchRByBounds
-- instead of binarySearchR, to avoid searching the first part
-- of the vector again.
-- | Returns a list of actions and the time remaining until they start.
findNextEvents :: Time -> Duration -> Time
               -> Museq -> (Duration, [Action])
findNextEvents time0 globalPeriod now museq =
  let period = globalPeriod * fromRational (_dur museq)
      pp0 = prevPhase0 time0 period now
      relNow = toRational $ (now - pp0) / period
      vecLen = V.length $ _vec museq
      compare' :: (RelDuration, a) -> (RelDuration, a) -> Ordering
      compare' ve ve' = compare (fst ve) (fst ve')
      dummyAction = New Boop "marge"
      start = firstIndexGTE         compare'
        (_vec museq) (relNow, dummyAction)
      end = if start < vecLen
            then lastIndexJustGTE
                 compare' (_vec museq) $ _vec museq ! start
            else vecLen
      relTimeOfNextEvent = if start < vecLen
                           then fst $ _vec museq ! start
                           else 1
      timeUntilNextEvent =
        fromRational relTimeOfNextEvent * period + pp0 - now
  in ( timeUntilNextEvent
     , map snd $ V.toList $ V.slice start (end - start) $ _vec museq)
