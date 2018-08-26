{-# LANGUAGE ScopedTypeVariables #-}

-- | = Mostly analysis

module Vivid.Jbb.Dispatch.Museq
  ( timeToPlayThrough
  , supsToPlayThrough
  , dursToPlayThrough
  , timeToRepeat
  , supsToRepeat
  , dursToRepeat

  , museqSynths
  , museqsDiff
  , sortMuseq
  , museqIsValid
  , longestDur
  , arc
  )
where

import Control.Lens ((^.),(.~),(%~),_1,_2,over,view)
import Control.Monad.ST
import Data.Fixed (div')
import Data.List ((\\))
import qualified Data.Map as M
import qualified Data.Vector as V
import           Data.Vector ((!))
import Data.Vector.Algorithms.Intro (sortBy)

import Vivid.Jbb.Dispatch.Types
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

timeToPlayThrough :: Museq a -> RTime
timeToPlayThrough m = RTime $ lcmRatios (tr $ _sup m) (tr $ _dur m)

supsToPlayThrough :: Museq a -> RTime
supsToPlayThrough m = timeToPlayThrough m / (_sup m)

dursToPlayThrough :: Museq a -> RTime
dursToPlayThrough m = timeToPlayThrough m / (_dur m)

timeToRepeat :: Museq a -> RTime
timeToRepeat m = let x = RTime $ lcmRatios (tr $ _sup m) (tr $ _dur m)
  in if x == _dur m then _sup m else x

supsToRepeat :: Museq a -> RTime
supsToRepeat m = timeToRepeat m / _sup m

dursToRepeat :: Museq a -> RTime
dursToRepeat m = timeToRepeat m / _dur m


-- | Given a Museq, find the synths it uses.
museqSynths :: Museq Action -> [(SynthDefEnum, SynthName)]
museqSynths = map (actionSynth . snd) . V.toList . _vec


-- | Given an old set of Museqs and a new one, figure out
-- which synths need to be created, and which destroyed.
-- PITFALL: Both resulting lists are ordered on the first element,
-- likely differing from either of the input maps.
museqsDiff :: M.Map MuseqName (Museq Action)
              -> M.Map MuseqName (Museq Action)
              -> ([(SynthDefEnum, SynthName)],
                  [(SynthDefEnum, SynthName)])
museqsDiff old new = (toFree,toCreate) where
  oldMuseqs = M.elems old :: [Museq Action]
  newMuseqs = M.elems new :: [Museq Action]
  oldSynths = unique $ concatMap museqSynths oldMuseqs
  newSynths = unique $ concatMap museqSynths newMuseqs
  toCreate = newSynths \\ oldSynths
  toFree = oldSynths \\ newSynths


-- | = Sort a Museq
sortMuseq :: Museq a -> Museq a
sortMuseq = vec %~
  \v -> runST $ do v' <- V.thaw v
                   let compare' ve ve' = compare (fst ve) (fst ve')
                   sortBy compare' v'
                   V.freeze v'

-- | A valid Museq' m is sorted on start and then end times,
-- with all end times >= the corresponding start times,
-- has (relative) duration > 0, and all events at time < _sup m.
-- (todo ? I'm not sure the end-time sort helps.)
-- PITFALL : The end times are permitted to be greater than the _sup.
museqIsValid :: Eq a => Museq a -> Bool
museqIsValid mu = and [a,b,c,d,e] where
  v = _vec mu
  a = if V.length v == 0 then True
      else (fst $ fst $ V.last v) < _sup mu
  b = mu == sortMuseq mu
  c = _dur mu > 0
  d = _sup mu > 0
  e = V.all (uncurry (<=) . fst) v

longestDur :: Museq a -> RDuration
longestDur m = let eventDur ((start,end),_) = end - start
               in V.maximum $ V.map eventDur $ _vec m

-- todo ? `arc` could be ~2x faster by using binarySearchRByBounds
-- instead of binarySearchR, to avoid searching the first part
-- of the vector again.
-- | Finds the events in [from,to).
arc :: forall a. Time -> Duration -> Time -> Time
     -> Museq a -> [((Time,Time), a)]
arc time0 tempoPeriod from to m =
  let period = tempoPeriod * tr (_sup m) :: Duration
      rdv = V.map (fst . fst) $ _vec $ const () <$> m :: V.Vector RTime
      latestPhase0 = prevPhase0 time0 period from :: Time
        -- it would be natural to start here, but long events from
        -- earlier cycles could carry into now, so we must back up
      earlierFrom = latestPhase0 - tr (longestDur m) * tempoPeriod :: Time
      oldestRelevantCycle = div' (earlierFrom - latestPhase0) period :: Int
      toAbsoluteTime :: (RTime,RTime) -> (Time,Time)
      toAbsoluteTime (a,b) = (f a, f b) where
        f rt = tr rt * tempoPeriod + latestPhase0
      chopStarts :: (Time,Time) -> (Time,Time)
      chopStarts = over _1 $ max from
      chopEnds :: (Time,Time) -> (Time,Time)
      chopEnds = over _2 $ min to
      dropImpossibles :: [((Time,Time),a)] -> [((Time,Time),a)]
        -- Because chopStarts can leave an old event starting after it ended.
      dropImpossibles = filter $ uncurry (<=) . fst
   in dropImpossibles
      $ map (over _1 $ chopEnds . chopStarts . toAbsoluteTime)
      $ arcFold oldestRelevantCycle period rdv time0 earlierFrom to m

arcFold :: forall a. Int -> Duration -> V.Vector RTime
  -> Time -> Time -> Time -- ^ the same three `Time` arguments as in `arc`
  -> Museq a -> [Ev a]
arcFold cycle period rdv time0 from to m =
  if from >= to then [] -- todo ? Be sure of `arc` boundary condition
  else let
    pp0 = prevPhase0 time0 period from :: Time
    fromInCycles = fr $ (from - pp0) / period :: RTime
    toInCycles   = fr $ (to   - pp0) / period :: RTime
    startOrOOBIndex = firstIndexGTE compare rdv $ fromInCycles * _sup m :: Int
  in if startOrOOBIndex >= V.length rdv
--     then let nextFrom = if pp0 + period > from
--    todo ? delete
-- -- If `from = pp0 + period - epsilon`, maybe `pp0 + period <= from`.
-- -- Thus floating point error used to make this if-then statement necessary
-- -- Now that all times are Rational, it's probably unnecessary.
--                         then pp0 + period
--                         else pp0 + 2*period
--          in arcFold (cycle+1) period rdv time0 nextFrom to m
     then arcFold (cycle+1) period rdv time0 (pp0 + period) to m
     else
       let startIndex = startOrOOBIndex :: Int
           endIndex = lastIndexLTE compare' rdv (toInCycles * _sup m) :: Int
             where 
             compare' x y = if x < y then LT else GT -- to omit the endpoint
           eventsThisCycle = V.toList
             $ V.map (over (_1._2) (+(_sup m * fromIntegral cycle)))
             $ V.map (over (_1._1) (+(_sup m * fromIntegral cycle)))
             $ V.slice startIndex (endIndex-startIndex) $ _vec m
       in eventsThisCycle
          ++ arcFold (cycle+1) period rdv time0 (pp0 + period) to m
