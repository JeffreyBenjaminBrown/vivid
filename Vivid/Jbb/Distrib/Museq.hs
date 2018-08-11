module Vivid.Jbb.Distrib.Museq where

import Control.Lens ((^.),(.~),(%~))
import Control.Monad.ST
import qualified Data.Vector as V
import           Data.Vector ((!))
import Data.Vector.Algorithms.Intro (sortBy)

import Vivid.Jbb.Distrib.Types
import Vivid.Jbb.Util
import Vivid.Jbb.Synths (SynthDefEnum(Boop))


-- | The length of time a Museq must play through to regain its initial state.
-- For example, if dur = 4 and sup = 6, 
-- then it will be ready to start all over 3 durs later, at time 12.
-- PITFALL ? TODO : If `sup m` divides `dur m`, it's already in the same
-- state after `sup m`, but this will produce `sup m`. The result is that
-- the output of `stack` can be redundant, usinig more space than necessary.
-- SOLUTION: Really there are two ways to repeat: One is that you're going
-- to sound the same. The other is that you've actually repeated the number
-- of times you said you would. The first is what stack ought to use;
-- the second is what append ought to use.
timeToRepeat :: Museq a -> Rational
timeToRepeat m = lcmRatios (_sup m) (_dur m)

-- | The number of its `sup`s a Museq must play through
-- to regain its initial state.
supsToRepeat :: Museq a -> Rational
supsToRepeat m = lcmRatios (_sup m) (_dur m) / _sup m

dursToRepeat :: Museq a -> Rational
dursToRepeat m = lcmRatios (_sup m) (_dur m) / _dur m

sortMuseq :: Museq a -> Museq a
sortMuseq = vec %~
  \v -> runST $ do v' <- V.thaw v
                   let compare' ve ve' = compare (fst ve) (fst ve')
                   sortBy compare' v'
                   V.freeze v'

-- | A valid Museq m is sorted on start time, has (relative) duration > 0,
-- and all actions at time < _sup m.
museqIsValid :: Eq a => Museq a -> Bool
museqIsValid mu = and [a,b,c,d] where
  a = if V.length (_vec mu) == 0 then True
      else fst (V.last $ _vec mu) < _sup mu
  b = mu == sortMuseq mu
  c = _dur mu > 0
  d = _sup mu > 0

-- todo ? `findNextEvents` could be slightly faster by using binarySearchRByBounds
-- instead of binarySearchR, to avoid searching the first part
-- of the vector again.
-- | Returns a list of actions and the time remaining until they start.
findNextEvents :: Time -> Duration -> Time
               -> Museq Action -> (Duration, [Action])
findNextEvents time0 globalPeriod now museq =
  let period = globalPeriod * fromRational (_sup museq)
      pp0 = prevPhase0 time0 period now
      relNow = toRational $ (now - pp0) / period
      vecLen = V.length $ _vec museq
      compare' :: (RelDuration, a) -> (RelDuration, a) -> Ordering
      compare' ve ve' = compare (fst ve) (fst ve')
      uv = _vec $ unitMuseq museq :: V.Vector (RelDuration,())
      startOrOOB = firstIndexGTE  compare' uv (relNow, ())
      start = if startOrOOB < vecLen then startOrOOB else 0
      end =     lastIndexJustGTE  compare' uv (uv ! start)
      relTimeOfNextEvent = if startOrOOB == start
                           then        fst $ uv ! start
                           else (+1) $ fst $ uv ! 0
      timeUntilNextEvent =
        fromRational relTimeOfNextEvent * period + pp0 - now
  in ( timeUntilNextEvent
     , map snd $ V.toList $ V.slice start (end - start) $ _vec museq )
