module Vivid.Jbb.Distrib.Museq (
  timeToPlayThrough
  , supsToPlayThrough
  , dursToPlayThrough
  , timeToRepeat
  , supsToRepeat
  , dursToRepeat

  , sortMuseq
  , museqIsValid
  , findNextEvents
  ) where

import Control.Lens ((^.),(.~),(%~))
import Control.Monad.ST
import qualified Data.Vector as V
import           Data.Vector ((!))
import Data.Vector.Algorithms.Intro (sortBy)

import Vivid.Jbb.Distrib.Types
import Vivid.Jbb.Util
import Vivid.Jbb.Synths (SynthDefEnum(Boop))


-- | = Figuring out when a Museq will repeat.
-- There are two senses in which a Museq can repeat. One is that
-- it sounds like it's repeating. If _dur = 2 and _sup = 1, then
-- it sounds like it's repeating after a single _sup.
-- The *ToRepeat functions below use that sense.
--
-- The other sense is that the Museq has cycled through what it
-- "is supposed to cycle through". (This is useful when `append`ing Museqs.)
-- If _dur = 2 and _sup = 1, it won't have played all the way through
-- until _dur has gone by, even though a listener hears it doing the same
-- thing halfway through that _dur.
-- The *ToPlayThrough functions below use that sense.
--
-- The results of the two families only differ when _sup divides _dur.
--
-- I could have used the *PlayThrough functions everywhere, but
-- in some situations that would waste space. For an example of one,
-- see in Tests.testStack the assertion labeled
-- "stack, where timeToRepeat differs from timeToPlayThrough".

timeToPlayThrough :: Museq a -> Rational
timeToPlayThrough m = lcmRatios (_sup m) (_dur m)

supsToPlayThrough :: Museq a -> Rational
supsToPlayThrough m = timeToPlayThrough m / _sup m

dursToPlayThrough :: Museq a -> Rational
dursToPlayThrough m = timeToPlayThrough m / _dur m

timeToRepeat :: Museq a -> Rational
timeToRepeat m = let x = lcmRatios (_sup m) (_dur m)
  in if x == _dur m then _sup m else x

supsToRepeat :: Museq a -> Rational
supsToRepeat m = timeToRepeat m / _sup m

dursToRepeat :: Museq a -> Rational
dursToRepeat m = timeToRepeat m / _dur m

-- | = Sort a Museq
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

-- todo ? `findNextEvents` could be ~2x faster by using binarySearchRByBounds
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
      uv = _vec $ unitMuseq museq :: V.Vector (RelDuration,())
      compare' :: (RelDuration, a) -> (RelDuration, a) -> Ordering
      compare' ve ve' = compare (fst ve) (fst ve')
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

---- todo ? `arc` could be ~2x faster by using binarySearchRByBounds
---- instead of binarySearchR, to avoid searching the first part
---- of the vector again.
--arc :: Time -> Duration -> Time -> Time
--    -> Museq Action -> (Duration, [Action])
--arc time0 globalPeriod from to museq =
--  let period = globalPeriod * fromRational (_sup museq)
--      pp0 = prevPhase0 time0 period from
--      relFrom = toRational $ (from - pp0) / period
--      relTo   = toRational $ (to   - pp0) / period
--      vecLen = V.length $ _vec museq
--      uv = _vec $ unitMuseq museq :: V.Vector (RelDuration,())
      
