{-# LANGUAGE ScopedTypeVariables #-}

-- | = Mostly analysis

module Vivid.Jbb.Dispatch.Museq
  (
  -- | = Make a Museq
  museq, museq0

  -- | = Timing
  , timeToPlayThrough
  , supsToPlayThrough
  , dursToPlayThrough
  , timeToRepeat
  , supsToRepeat
  , dursToRepeat

  , longestDur

  -- | = Naming events
  , museqMaybeNamesAreValid
  , nameAnonEvents
  , unusedName
  , intNameEvents

  -- | = More
  , museqSynths
  , museqsDiff
  , sortMuseq
  , museqIsValid
  , arc
  )
where

import Control.Lens ((^.),(.~),(%~),_1,_2,over,view)
import Control.Monad.ST
import Data.Fixed (div')
import Data.Function (on)
import qualified Data.List as L
import qualified Data.Maybe as Mb
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
-- | = Make a Museq

-- | Make a Museq, specifying start and end times
museq :: RDuration -> [((Rational,Rational),a)] -> Museq a
museq d tas = sortMuseq $ Museq { _dur = d
                                , _sup = d
                                , _vec = V.fromList $ map (over _1 f) tas }
  where f (start,end) = (fr start, fr end)


-- | Make a Museq of instantaneous events, specifying only start times
museq0 :: RDuration -> [(RTime,a)] -> Museq a
museq0 d tas = sortMuseq $ Museq {_dur = d, _sup = d,
                                  _vec = V.fromList $ map f tas}
  where f (t,val) = ((t,t),val)


-- | = Timing
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


longestDur :: Museq a -> RDuration
longestDur m = let eventDur ((start,end),_) = end - start
               in V.maximum $ V.map eventDur $ _vec m


-- | = Naming events

-- | The names in a `Museq Name` are valid if no events with the same
-- name overlap in time, within or across cycles.
museqMaybeNamesAreValid :: forall a t. Eq t
  => Museq (NamedWith (Maybe t) a) -> Bool
museqMaybeNamesAreValid m = and $ map goodGroup nameGroups where
  namedEvents = filter (Mb.isJust . fst . snd) $ V.toList $ _vec m
  nameGroups = L.groupBy eq' namedEvents :: [[Ev (NamedWith (Maybe t) a)]]
    where eq' (_,(name,_)) (_,(name',_)) = name == name'
  noAdjacentOverlap, noWrappedOverlap, goodGroup
    :: [Ev (NamedWith (Maybe t) a)] -> Bool
  noAdjacentOverlap [] = True
  noAdjacentOverlap (a:[]) = True
  noAdjacentOverlap (a@((_,e),_) : b@((s,_),_) : more) =
    s >= e && noAdjacentOverlap (b:more)
  noWrappedOverlap evs = let ((s,_),_) = head evs
                             ((_,e),_) = last evs
                         in s + _sup m >= e
  goodGroup g = noAdjacentOverlap g && noWrappedOverlap g

unusedName :: [String] -> String
unusedName names = head $ (L.\\) allStrings names where
  allStrings = [ c : s | s <- "" : allStrings
                       , c <- ['a'..'z'] ++ ['0'..'9'] ]

nameAnonEvents :: forall a.
                  Museq (NamedWith (Maybe String) a)
               -> Museq (NamedWith String a)
nameAnonEvents m = let
  evs, namedEvs, anonEvs :: [Ev (NamedWith (Maybe String) a)]
  evs = V.toList $ _vec m 
  (namedEvs, anonEvs) = L.partition (Mb.isJust . fst . snd) evs
  namedEvs', namedAnons :: [Ev (NamedWith String a)]
  namedEvs' = map (over (_2._1) Mb.fromJust) namedEvs
  names = Mb.catMaybes $ map (fst . snd) namedEvs :: [String]
  anonEvs' = map (over _2 snd) anonEvs :: [Ev a]
  intNamedAnons = intNameEvents (_sup m) anonEvs'
    :: [Ev (NamedWith Int a)]
  namedAnons = map (over (_2._1) f) intNamedAnons where
    f = ((++) $ unusedName names) . show
    -- The ++ ensures no name conflicts.
    -- todo : learn Prisms, use _Just
  in sortMuseq $ m {_vec = V.fromList $ namedEvs' ++ namedAnons}

-- | Assign a minimal number of names (which are integers in string form), 
-- starting from 1, so that like-named events do not overlap.
-- ASSUMES the input list is sorted on (start,end) times.
intNameEvents :: RDuration -- ^ _sup of the Museq these Evs come from
              -> [Ev a] -- ^ these are being named
              -> [Ev (NamedWith Int a)]
intNameEvents len ( ((s1,e1),a1) : more ) = 
  ((s1,e1),(1,a1))
  : intNameEvents' len (s1,(1, a1)) [(e1,(1,a1))] more

intNameEvents' :: forall a t.
  RDuration -- ^ _sup of the Museq these Evs come from
  -> (RTime,NamedWith Int a) -- ^ (start, name) of the first event
  -> [(RTime,NamedWith Int a)] -- ^ (end, name)s of ongoing events
  -> [Ev a] -- ^ these are being named
  -> [Ev (NamedWith Int a)]
intNameEvents' _ _ _ [] = []
intNameEvents' sup ev1@(s1,(i1,a1)) ongoing (((s,e),a) : more) = let
  -- Handles ((s,t),a), then recurses.
  -- Acronyms: s = start, e = end, mn = maybe-name, n = name
  ongoing' = filter (\(e',_) -> e' >= s) ongoing
    -- ongoing in the sense that they overlap (s,e)
  firstOverlaps = s1 + sup <= e
    -- todo speed ? ongoing' and `firstOverlaps` are conservative.
    -- For events with duration > 0, (e' > s) would work.
  overlappingMaybeNames = if firstOverlaps then i1 : is else is where
    is = map (fst . snd) ongoing'
  name = head $ (L.\\) [1..] overlappingMaybeNames
  in ((s,e),(name,a)) : intNameEvents' sup ev1 ongoing' more


-- | = More
-- | Given a Museq, find the synths it uses.

museqSynths :: Museq Note -> [(SynthDefEnum, SynthName)]
museqSynths m = map (f . snd) evs where
  evs = V.toList $ _vec m :: [Ev Note]
  f :: Note -> (SynthDefEnum, SynthName)
  f (name,(sde,msg)) = (sde,name)

-- | Given an old set of Museqs and a new one, figure out
-- which synths need to be created, and which destroyed.
-- PITFALL: Both resulting lists are ordered on the first element,
-- likely differing from either of the input maps.
museqsDiff :: M.Map MuseqName (Museq Note)
              -> M.Map MuseqName (Museq Note)
              -> ([(SynthDefEnum, SynthName)],
                  [(SynthDefEnum, SynthName)])
museqsDiff old new = (toFree,toCreate) where
  oldMuseqs = M.elems old :: [Museq Note]
  newMuseqs = M.elems new :: [Museq Note]
  oldSynths = unique $ concatMap museqSynths oldMuseqs
  newSynths = unique $ concatMap museqSynths newMuseqs
  toCreate = (L.\\) newSynths oldSynths
  toFree = (L.\\) oldSynths newSynths

-- | = Sort a Museq
sortMuseq :: Museq a -> Museq a
sortMuseq = vec %~
  \v -> runST $ do v' <- V.thaw v
                   let compare' ve ve' = compare (fst ve) (fst ve')
                   sortBy compare' v'
                   V.freeze v'

sortMuseq' :: Museq' l a -> Museq' l a
sortMuseq' = vec' %~
  \v -> runST $ do v' <- V.thaw v
                   sortBy (compare `on` view evStart) v'
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
-- -- todo ? delete
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
