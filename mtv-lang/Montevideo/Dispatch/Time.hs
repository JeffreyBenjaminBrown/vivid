module Montevideo.Dispatch.Time (
    museqFrame -- ^ Museq String ScAction
               -- -> [(Time, ScAction)] -- ^ `Time`s are start times
  , arc -- ^ forall l a. Time -> Duration -> Time -> Time
        -- -> Museq l a -> [Event Time l a]
  , nextPhase0 -- ^ RealFrac a => a -> a -> a -> a
  , prevPhase0 -- ^ RealFrac a => a -> a -> a -> a

  -- | = Timing a Museq
  , supsToAppearToFinish -- ^ Museq l a -> RTime
  , dursToAppearToFinish -- ^ Museq l a -> RTime
  , longestDur           -- ^ Museq l a -> RDuration
  , supsToFinish         -- ^ Museq l a -> RTime
  , dursToFinish         -- ^ Museq l a -> RTime
  , timeToAppearToFinish -- ^ Museq l a -> RTime
  , timeToFinish         -- ^ Museq l a -> RTime
  ) where

import Prelude hiding (cycle)
import Control.Lens hiding (to,from)
import Data.Fixed (div',mod')
import qualified Data.Vector as V

import Montevideo.Dispatch.Config (frameDuration)
import Montevideo.Dispatch.Types
import Montevideo.Synth.Msg
import Montevideo.Util


museqFrame :: forall a.
     Time     -- ^ time0, a historical reference point
  -> Duration -- ^ tempo period
  -> Time     -- ^ when to start rendering
  -> Museq String (ScAction a) -- ^ what to pluck events from
  -> [(Time, ScAction a)]      -- ^ the `Time`s are start times
museqFrame time0 tempoPeriod start m = let
  evs :: [Event Time String (ScAction a)] =
    arc time0 tempoPeriod start
    (start + frameDuration) m
  in map (\ev -> ((ev^.evStart), (ev^.evData))) evs

-- | `arc time0 period from to m`
-- finds the events of `m` in the half-open interval `[from,to)`.
-- PITFALL: Although this goes to great painns to consider events from
-- earlier cycles, I'm not sure it makes sense to have an event with a
-- duration longer than the Museq's period, because then it will overlap
-- with future renderings of itself.
arc :: forall l a.
    Time       -- ^ a reference point in the past
  -> Duration  -- ^ tempo period
  -> Time      -- ^ start here (inclusive)
  -> Time      -- ^ end here (not inclusive)
  -> Museq l a -- ^ pluck Events from here
  -> [Event Time l a]
-- todo ? `arc` could be ~2x faster by using binarySearchRByBounds
-- instead of binarySearchR, to avoid searching the first part
-- of the vector again.

arc time0 tempoPeriod from to m = let
  period       :: Duration       = tempoPeriod * Time (_unRTime $ _sup m)
  startVec     :: V.Vector RTime = V.map (view evStart) $ _vec $ m
  latestPhase0 :: Time           = prevPhase0 time0 period from

  -- The earliest start time for which an event from `m`
  -- might still be happening.
  oldestRelevantPhase0 :: Time =
    latestPhase0 - (Time $ _unRTime $ longestDur m) * tempoPeriod
  -- A non-positive number.
  -- If it is -1, we must look 1 cycle into the past.
  oldestRelevantCycle :: Int =
    div' (oldestRelevantPhase0 - latestPhase0) period

  makeTimesAbsolute :: (RTime,RTime) -> (Time,Time)
  makeTimesAbsolute (a,b) = (f a, f b) where
    f rt = Time (_unRTime rt) * tempoPeriod + latestPhase0

  -- If an event to be rendered started before this frame,
  -- the portion before is discarded.
  truncateStarts :: (Time,Time) -> (Time,Time)
  truncateStarts = over _1 $ max from
  -- If an event to be rendered ends after this frame,
  -- the portion after is discarded.
  truncateEnds :: (Time,Time) -> (Time,Time)
  truncateEnds = over _2 $ min to

  -- Because truncateStarts can leave an old event starting after it ended.
  -- (Starting at the same time as ending is fine.)
  dropImpossibles :: [Event Time l a] -> [Event Time l a]
  dropImpossibles = filter $ uncurry (<=) . view evArc

  futzTimes :: Event RTime l a -> Event Time l a
  futzTimes ev = over evArc f ev
    where f = truncateEnds . truncateStarts . makeTimesAbsolute
  in dropImpossibles $ map futzTimes $ _arcFold
     oldestRelevantCycle period startVec time0 oldestRelevantPhase0 to m

-- | Most of these arguments are constant across `_arcFold`'s
-- recursive calls to itself. The exceptions are `cycle` and `from`.
_arcFold :: forall l a.
     Int -- ^ The cycle being operated on. Advances by 1 each recursive call.
  -> Duration       -- ^ Tempo period.
  -> V.Vector RTime -- ^ A vector (sorted, I think) of start times.
  -> Time           -- ^ time0, historical reference point.
  -> Time -- ^ Start here. The fold will work its way from here to the start
          -- of the current frame, advancing by `period`s each time,
          -- then one more. Then it stops, because `from >= to`.
          -- Does not need to start on a tempo period boundary.
          -- After the first call, it always advances by the tempo period;
          -- that first time it usually advances by less.
  -> Time -- ^ End here.
  -> Museq l a -> [Ev l a]

_arcFold cycle period startVec time0 from to m =
  if null (m ^. vec) || from >= to
    -- TODO ? Be sure of `arc` boundary condition
  then [] else let
    -- When arc first calls _arcFold, the `from` argument need not be
    -- on a period boundary. In every successive call to _arcFold, I think,
    -- it will be.
    pp0          ::  Time = prevPhase0 time0 period from
    fromInCycles :: RTime = RTime . _unTime $ (from - pp0) / period
    toInCycles   :: RTime = RTime . _unTime $ (to   - pp0) / period

    -- Hypothesis: This is to ensure that on the first call
    -- to `_arcFold`, if `from` is after the start of all events,
    -- that cycle is skipped. (On subsequent calls, `from` will always
    -- be on a period boundary, so this will always be 0.)
    startOrOOBIndex ::   Int = -- OOB = out of bounds, I think
      firstIndexGTE compare startVec $ fromInCycles * _sup m

  in if startOrOOBIndex >= V.length startVec
-- -- todo ? delete
-- -- If `from = pp0 + period - epsilon`, maybe `pp0 + period <= from`.
-- -- Thus floating point error used to make this if-then statement necessary
-- -- Now that all times are Rational, it's probably unnecessary.
--     then let nextFrom = if pp0 + period > from
--                         then pp0 + period
--                         else pp0 + 2*period
--          in _arcFold (cycle+1) period startVec time0 nextFrom to m
     then _arcFold (cycle+1) period startVec time0 (pp0 + period) to m
     else let

       startIndex = startOrOOBIndex :: Int
       endIndex = lastIndexLTE compare' startVec
                  $ toInCycles * _sup m :: Int
         where compare' x y =
                 if x < y then LT else GT -- to omit the endpoint
       eventsThisCycle = V.toList
         $ V.map (over evEnd   (+(_sup m * fromIntegral cycle)))
         $ V.map (over evStart (+(_sup m * fromIntegral cycle)))
         $ V.slice startIndex (endIndex-startIndex) $ _vec m
       in eventsThisCycle
          ++ _arcFold (cycle+1) period startVec time0 (pp0 + period) to m

-- | `nextPhase0 time0 period now` finds the least time `t`
-- greater than or equal to `now` such that for some integer `n`,
-- `t = n * period + time0`.
nextPhase0 :: forall a. RealFrac a
           => a -- ^ the first time that had phase 0
           -> a -- ^ the duration of a cycle
           -> a -- ^ the time right now
           -> a
nextPhase0 time0 period now =
  -- This might be a little over-optimized.
  -- It'd be simpler to write this in terms of `prevPhase0`,
  -- but then it would calculate `relativeNow` twice.
  let relativeNow        :: a    = now - time0
      wholeElapsedCycles :: Int  = div' relativeNow period
      onBoundary         :: Bool = mod' relativeNow period == 0
  in time0 + period * ( fi wholeElapsedCycles +
                        if onBoundary then 0 else 1 )

-- | PITFALL ? if `lastPhase0` or `nextPhase0`
-- was called precisely at `phase0`,
-- both would yield the same result, namely `phase0`.
-- Time is measured in microseconds,
-- so there is a one in a million chance of that.
prevPhase0 :: RealFrac a => a -> a -> a -> a
prevPhase0 time0 period now =
  fromIntegral x * period + time0
  where x :: Int = div' (now - time0) period


-- | = Timing a Museq

-- | `longestDur m` is the duration of the longest event in `m`.
longestDur :: Museq l a -> RDuration
longestDur m = let eventDur ev = (ev ^. evEnd) - (ev ^. evStart)
               in V.maximum $ V.map eventDur $ _vec m

-- | = Figuring out when a Museq will repeat.
-- There are two senses in which a Museq can repeat. One is that
-- it sounds like it's repeating. If _dur = 2 and _sup = 1, then
-- it sounds like it's repeating after a single _sup.
-- The *ToRepeat functions below use that sense.
--
-- The other sense is that the Museq has cycled through what it
-- "is supposed to cycle through". (This is useful when `append`ing Museqs.)
-- If _dur = 2 and _sup = 1, it won't have played all the way through
-- until _dur has gone by, even though a listener hears it start to repeat
-- halfway through that _dur.
-- The *ToPlayThrough functions below use that sense.
--
-- The results of the two families only differ when _sup divides _dur.
--
-- I could have used the *PlayThrough functions everywhere, but
-- in some situations that would waste space. For an example of one,
-- see in Tests.testStack the assertion labeled
-- "stack, where timeToAppearToFinish differs from timeToFinish".

supsToAppearToFinish :: Museq l a -> RTime
supsToAppearToFinish m = timeToAppearToFinish m / _sup m

dursToAppearToFinish :: Museq l a -> RTime
dursToAppearToFinish m = timeToAppearToFinish m / _dur m

supsToFinish :: Museq l a -> RTime
supsToFinish m = timeToFinish m / (_sup m)

dursToFinish :: Museq l a -> RTime
dursToFinish m = timeToFinish m / (_dur m)

-- | After `timeToAppearToFinish`, the `Museq` *sounds* like it's repeating.
-- That doesn't mean it's played all the way through, though.
timeToAppearToFinish :: Museq l a -> RTime
timeToAppearToFinish m =
  let tp = timeToFinish m
                 in if tp == _dur m -- implies `_dur m > _sup m`
                    then _sup m else tp

-- | A `Museq` has "finished" when it has played for a time
-- that is an integer multiple of both _dur and _sup.
-- Remember, if it is concatenated to another pattern,
-- it is not treated as playing when the other one sounds.
timeToFinish :: Museq l a -> RTime
timeToFinish m = RTime $ lcmRatios (tr $ _sup m) (tr $ _dur m)
