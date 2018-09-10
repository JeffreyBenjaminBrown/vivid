{-# LANGUAGE ScopedTypeVariables #-}

-- | = Mostly analysis

module Vivid.Jbb.Dispatch.Museq
  (
  -- | = Make a Museq
  museq, museq0
  , museq'

  -- | = Timing
  , timeToPlayThrough
  , supsToPlayThrough
  , dursToPlayThrough
  , timeToRepeat
  , supsToRepeat
  , dursToRepeat

  , timeToPlayThrough'
  , supsToPlayThrough'
  , dursToPlayThrough'
  , timeToRepeat'
  , supsToRepeat'
  , dursToRepeat'

  , longestDur
  , longestDur'

  -- | = Naming events
  , museqMaybeNamesAreValid
  , museqMaybeNamesAreValid'
  , nameAnonEvents
  , nameAnonEvents'
  , intNameEvents
  , intNameEvents'

  -- | = More
  , museqSynths
  , museqSynths'
  , museqsDiff
  , museqsDiff'
  , sortMuseq
  , sortMuseq'
  , museqIsValid
  , museqIsValid'
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
museq0 :: RDuration -> [(Rational,a)] -> Museq a
museq0 d tas = sortMuseq $ Museq {_dur = d, _sup = d,
                                  _vec = V.fromList $ map f tas}
  where f (t,val) = ((fr t, fr t), val)


-- | Make a Museq', specifying start and end times
museq' :: RDuration -> [Ev' l a] -> Museq' l a
museq' d evs = sortMuseq' $ Museq' { _dur' = d
                                   , _sup' = d
                                   , _vec' = V.fromList $ evs }


-- | = Timing a Museq
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


-- | = Timing a Museq'
timeToPlayThrough' :: Museq' l a -> RTime
timeToPlayThrough' m = RTime $ lcmRatios (tr $ _sup' m) (tr $ _dur' m)

supsToPlayThrough' :: Museq' l a -> RTime
supsToPlayThrough' m = timeToPlayThrough' m / (_sup' m)

dursToPlayThrough' :: Museq' l a -> RTime
dursToPlayThrough' m = timeToPlayThrough' m / (_dur' m)

timeToRepeat' :: Museq' l a -> RTime
timeToRepeat' m = let x = RTime $ lcmRatios (tr $ _sup' m) (tr $ _dur' m)
  in if x == _dur' m then _sup' m else x

supsToRepeat' :: Museq' l a -> RTime
supsToRepeat' m = timeToRepeat' m / _sup' m

dursToRepeat' :: Museq' l a -> RTime
dursToRepeat' m = timeToRepeat' m / _dur' m

longestDur :: Museq a -> RDuration
longestDur m = let eventDur ((start,end),_) = end - start
               in V.maximum $ V.map eventDur $ _vec m

longestDur' :: Museq' l a -> RDuration
longestDur' m = let eventDur ev = (ev ^. evStart) - (ev ^. evEnd)
                in V.maximum $ V.map eventDur $ _vec' m


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

museqMaybeNamesAreValid' :: forall l a. Eq l
                         => Museq' (Maybe l) a -> Bool
museqMaybeNamesAreValid' m = and $ map goodGroup nameGroups where
  namedEvents = filter (Mb.isJust . view evLabel) $ V.toList $ _vec' m
  nameGroups = L.groupBy ((==) `on` view evLabel) namedEvents
    :: [[Ev' (Maybe l) a]]
  adjacentOverlap, wrappedOverlap, goodGroup
    :: [Ev' (Maybe l) a] -> Bool
  adjacentOverlap [] = False
  adjacentOverlap (a:[]) = False
  adjacentOverlap (e : f : more) = overlap (e^.evArc) (f^.evArc)
                                   || adjacentOverlap (f:more)
  wrappedOverlap evs = overlap (last evs ^. evArc)
                       (bumpArc (_sup' m) $ head evs ^. evArc)
  goodGroup g = not $ adjacentOverlap g || wrappedOverlap g

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

nameAnonEvents' :: forall a.
                   Museq' (Maybe String) a
                -> Museq'        String  a
nameAnonEvents' m =
  sortMuseq' $ m {_vec' = V.fromList $ namedEvs' ++ namedAnons}
  where
    evs, namedEvs, anonEvs :: [Ev' (Maybe String) a]
    evs = V.toList $ _vec' m
    (namedEvs, anonEvs) = L.partition (Mb.isJust . view evLabel) evs
    namedEvs' = map (over evLabel Mb.fromJust) namedEvs :: [Ev' String a]
    names = map (view evLabel) namedEvs' :: [String]
    anonEvs' = map (over evLabel $ const ()) anonEvs :: [Ev' () a]
    intNamedAnons = intNameEvents' (_sup' m) anonEvs' :: [Ev' Int a]
    namedAnons = map (over evLabel f) intNamedAnons :: [Ev' String a]
      where f = ((++) $ unusedName names) . show
            -- The ++ ensures no name conflicts.

-- | Assign a minimal number of names (which are integers in string form),
-- starting from 1, so that like-named events do not overlap.
-- ASSUMES the input list is sorted on (start,end) times.
intNameEvents :: RDuration -- ^ _sup of the Museq these Evs come from
              -> [Ev a] -- ^ these are being named
              -> [Ev (NamedWith Int a)]
intNameEvents len ( ((s1,e1),a1) : more ) =
  ((s1,e1),(1,a1))
  : _intNameEvents len (s1,(1, a1)) [(e1,(1,a1))] more

intNameEvents' :: RDuration -- ^ _sup of the Museq these Evs come from
               -> [Ev' () a] -- ^ these are being named
               -> [Ev' Int a]
intNameEvents' sup (ev1:more) = ev1' : _intNameEvents' sup ev1' [ev1'] more
  where ev1' = over evLabel (const 1) ev1

_intNameEvents :: forall a t.
  RDuration -- ^ _sup of the Museq these Evs come from
  -> (RTime,NamedWith Int a) -- ^ (start, name) of the first event
  -> [(RTime,NamedWith Int a)] -- ^ (end, name)s of ongoing events
  -> [Ev a] -- ^ these are being named
  -> [Ev (NamedWith Int a)]
_intNameEvents _ _ _ [] = []
_intNameEvents sup ev1@(s1,(i1,a1)) ongoing (((s,e),a) : more) = let
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
  in ((s,e),(name,a)) : _intNameEvents sup ev1 ongoing' more

_intNameEvents' :: forall a t.
  RDuration -- ^ _sup of the Museq these Evs come from
  -> (Ev' Int a) -- ^ the first event
  -> [Ev' Int a] -- ^ ongoing events
  -> [Ev' ()  a] -- ^ these are being named
  -> [Ev' Int a]
_intNameEvents' _ _ _ [] = []
_intNameEvents' sup ev1 ongoing (ev : more) = over evLabel (const name) ev
    : _intNameEvents' sup ev1 ongoing' more
  where
    ongoing' = filter (\ev' -> overlap (ev ^. evArc) (ev' ^. evArc)) ongoing
      -- ongoing in the sense that they do not end before ev starts
    firstOverlaps = overlap (bumpArc sup $ ev1 ^. evArc) (ev ^. evArc)
    overlappingMaybeNames = if firstOverlaps
                            then (ev1 ^. evLabel) : ns
                            else                    ns
      where ns = map (view evLabel) ongoing'
    name = head $ (L.\\) [1..] overlappingMaybeNames


-- | = More
-- | Given a Museq, find the synths it uses.

museqSynths :: Museq Note -> [(SynthDefEnum, SynthName)]
museqSynths m = map (f . snd) evs where
  evs = V.toList $ _vec m :: [Ev Note]
  f :: Note -> (SynthDefEnum, SynthName)
  f (name,(sde,msg)) = (sde,name)

museqSynths' :: Museq' String Note' -> [(SynthDefEnum, SynthName)]
museqSynths' m = map f evs where
  evs = V.toList $ _vec' m :: [Ev' String Note']
  f :: Ev' String Note' -> (SynthDefEnum, SynthName)
  f ev = ( view (evData . noteSd) ev
         , view evLabel ev )


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

museqsDiff' :: M.Map MuseqName (Museq' String Note')
            -> M.Map MuseqName (Museq' String Note')
            -> ([(SynthDefEnum, SynthName)],
                 [(SynthDefEnum, SynthName)])
museqsDiff' old new = (toFree,toCreate) where
  oldMuseqs = M.elems old :: [Museq' String Note']
  newMuseqs = M.elems new :: [Museq' String Note']
  oldSynths = unique $ concatMap museqSynths' oldMuseqs
  newSynths = unique $ concatMap museqSynths' newMuseqs
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

museqIsValid' :: (Eq a, Eq l) => Museq' l a -> Bool
museqIsValid' mu = and [a,b,c,d,e] where
  v = _vec' mu
  a = if V.length v == 0 then True
      else (view evEnd $ V.last v) < _sup' mu
  b = mu == sortMuseq' mu
  c = _dur' mu > 0
  d = _sup' mu > 0
  e = V.all (uncurry (<=) . view evArc) v

-- todo ? `arc` could be ~2x faster by using binarySearchRByBounds
-- instead of binarySearchR, to avoid searching the first part
-- of the vector again.
-- | Finds the events in [from,to).
arc :: forall a. Time -> Duration -> Time -> Time
     -> Museq a -> [((Time,Time), a)]
arc time0 tempoPeriod from to m =
  let period = tempoPeriod * tr (_sup m) :: Duration
      startVec = V.map (fst . fst) $ _vec $ const () <$> m :: V.Vector RTime
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
      $ _arcFold oldestRelevantCycle period startVec time0 earlierFrom to m

_arcFold :: forall a. Int -> Duration -> V.Vector RTime
  -> Time -> Time -> Time -- ^ the same three `Time` arguments as in `arc`
  -> Museq a -> [Ev a]
_arcFold cycle period startVec time0 from to m =
  if from >= to then [] -- todo ? Be sure of `arc` boundary condition
  else let
    pp0 = prevPhase0 time0 period from :: Time
    fromInCycles = fr $ (from - pp0) / period :: RTime
    toInCycles   = fr $ (to   - pp0) / period :: RTime
    startOrOOBIndex =
      firstIndexGTE compare startVec $ fromInCycles * _sup m :: Int
  in if startOrOOBIndex >= V.length startVec
--     then let nextFrom = if pp0 + period > from
-- -- todo ? delete
-- -- If `from = pp0 + period - epsilon`, maybe `pp0 + period <= from`.
-- -- Thus floating point error used to make this if-then statement necessary
-- -- Now that all times are Rational, it's probably unnecessary.
--                         then pp0 + period
--                         else pp0 + 2*period
--          in _arcFold (cycle+1) period startVec time0 nextFrom to m
     then _arcFold (cycle+1) period startVec time0 (pp0 + period) to m
     else
       let startIndex = startOrOOBIndex :: Int
           endIndex = lastIndexLTE compare' startVec
                      $ toInCycles * _sup m :: Int
             where compare' x y =
                     if x < y then LT else GT -- to omit the endpoint
           eventsThisCycle = V.toList
             $ V.map (over (_1._2) (+(_sup m * fromIntegral cycle)))
             $ V.map (over (_1._1) (+(_sup m * fromIntegral cycle)))
             $ V.slice startIndex (endIndex-startIndex) $ _vec m
       in eventsThisCycle
          ++ _arcFold (cycle+1) period startVec time0 (pp0 + period) to m
