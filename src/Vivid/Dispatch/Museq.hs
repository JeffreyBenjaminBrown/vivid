{-# LANGUAGE ScopedTypeVariables #-}

-- | = Mostly analysis

module Vivid.Dispatch.Museq (
  -- | = Make a Museq
    mkMuseq   -- ^ RDuration -> [Ev l a]            -> Museq l a
  , mkMuseqH  -- ^ forall a l. Ord l
               -- => RDuration -> [(l,RDuration,a)]   -> Museq l a
  , mkMuseqHo -- ^ forall a l. Ord l
               -- => RDuration -> [(l,RDuration,Msg)] -> Museq l Msg
  , mkMuseqRt -- ^ forall l. (Ord l, Show l)
    -- => RDuration -> [(l,RTime,Sample,Msg)] -> Museq String Note
  , mkMuseqRt1 -- ^ RDuration -> [(RTime,Sample)] -> Museq String Note
  , hold       -- ^ Num t => t -> [(t,a)] -> [((t,t),a)]
  , insertOffs -- ^ Museq l Msg -> Museq l Msg
  , insertOns  -- ^ Museq l Msg -> Museq l Msg

  -- | = Timing
  , timeToPlayThrough -- ^ Museq l a -> RTime
  , supsToPlayThrough -- ^ Museq l a -> RTime
  , dursToPlayThrough -- ^ Museq l a -> RTime
  , timeToRepeat      -- ^ Museq l a -> RTime
  , supsToRepeat      -- ^ Museq l a -> RTime
  , dursToRepeat      -- ^ Museq l a -> RTime
  , longestDur -- ^ Museq l a -> RDuration

  -- | = Naming events
  , labelsToStrings -- ^ Show l => Museq l a -> Museq String a
  , museqMaybeNamesAreValid -- ^ forall l a. Eq l
       -- => Museq (Maybe l) a -> Bool
  , nameAnonEvents -- ^ forall a.
                --    Museq (Maybe String) a
                -- -> Museq        String  a
  , intNameEvents -- ^ RDuration -- ^ _sup of the Museq these Evs come from
                  -- -> [Ev () a] -- ^ these are being named
                  -- -> [Ev Int a]

  -- | = More
  , museqSynths -- ^ Museq String Note -> [(SynthDefEnum, SynthName)]
  , museqsDiff -- ^ M.Map MuseqName (Museq String Note)
               -- -> M.Map MuseqName (Museq String Note)
               -- -> ([(SynthDefEnum, SynthName)],
               --     [(SynthDefEnum, SynthName)])
  , sortMuseq -- ^ Museq l a -> Museq l a
  , museqIsValid -- ^ (Eq a, Eq l) => Museq l a -> Bool
  , arc -- ^ forall l a. Time -> Duration -> Time -> Time
        -- -> Museq l a -> [Event Time l a]
  ) where

import Prelude hiding (cycle)

import Control.Lens hiding (to,from)
import Control.Monad.ST
import Data.Fixed (div')
import Data.Function (on)
import qualified Data.List as L
import qualified Data.Maybe as Mb
import qualified Data.Map as M
import qualified Data.Vector as V
import Data.Vector.Algorithms.Intro (sortBy)

import Vivid.Dispatch.Types
import Util
import Vivid.Synths
import Vivid.Synths.Samples
import Vivid.Synths (SynthDefEnum)


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
mkMuseq :: RDuration -> [Ev l a] -> Museq l a
mkMuseq d evs = sortMuseq $ Museq { _dur = d
                                  , _sup = d
                                  , _vec = V.fromList $ evs }

mkMuseq_seqProc :: forall a b l. Ord l
  => ([(RDuration,a)] -> [((RDuration,RDuration),b)])
  -> RDuration -> [(l,RDuration,a)] -> Museq l b
mkMuseq_seqProc seqProc d evs0 = let
  evs1 :: [(l,(RDuration,a))] =
    map (\(l,t,a) -> (l,(t,a))) evs0
  evs2 :: [(l,[(RDuration,a)])] =
    multiPartition evs1
  evs3 :: [(l,[((RDuration,RDuration),b)])] =
    map (_2 %~ seqProc) evs2
  evs4 :: [Ev l b] = concatMap f evs3 where
    f (l,ttas) = map g ttas where
      g :: ((RDuration,RDuration),b) -> Ev l b
      g ((t,s),a) = Event { _evLabel = l
                          , _evArc = (t,s)
                          , _evData = a }
  in mkMuseq d evs4

mkMuseqH :: forall a l. Ord l
          => RDuration -> [(l,RDuration,a)] -> Museq l a
mkMuseqH d = mkMuseq_seqProc (hold d) d

-- | Like `mkMuseqH`, but inserts `on = 1` in `Event`s that do not
-- mention the `on` parameter. Specialized to `Msg` payloads.
mkMuseqHo :: forall l. Ord l
          => RDuration -> [(l,RDuration,Msg)] -> Museq l Msg
mkMuseqHo d evs0 = insertOns $ mkMuseqH d evs0

-- | `mkMuseqRt` sends any two `Msg` values to different synths, unless
-- they share the same label *and* the same `Sample`.
-- This is guaranteed by computing new labels `show l ++ show Sample`.
--
-- Each (time,Msg) pair must become a pair of Msgs,
-- in order for retriggering to work.
-- `prepareToRetrigger` does that.

mkMuseqRt :: forall l. (Ord l, Show l) =>
  RDuration -> [(l,RTime,Sample,Msg)] -> Museq String Note
mkMuseqRt sup0 evs0 = let
  -- rather than group by l and then Sample,
  -- maybe group by l' = show l ++ show Sample?
  evs1 :: [((l, Sample), (RTime,Msg)) ] =
    map (\(l,t,n,m) -> ((l,n),(t,m))) evs0
  evs2 :: [( (l, Sample), [(RTime,Msg)] )] =
    multiPartition evs1
  evs3 :: [( (l,Sample), [((RTime,RTime), Msg )] )] =
    map (_2 %~ prepareToRetrigger sup0) evs2
  evs4 :: [( (l,Sample), [((RTime,RTime), Note)] )] =
    map f evs3
    where f :: ( (l,Sample), [((RTime,RTime), Msg )] )
            -> ( (l,Sample), [((RTime,RTime), Note)] )
          f ((l,s),pairs) =
            ((l,s), map (_2 %~ Note (Sampler s)) pairs)
  evs5 :: [( String, [((RTime,RTime), Note)] )] =
    map (_1 %~ \(l,s) -> show l ++ show s) evs4
  evs6 :: [Event RTime String Note] =
    concatMap (\(s,ps) -> map (\(ts,n) -> Event s ts n) ps) evs5
  in mkMuseq sup0 evs6

mkMuseqRt1 :: RDuration -> [(RTime,Sample)] -> Museq String Note
mkMuseqRt1 sup0 = mkMuseqRt sup0 . map f where
  f (t,s) = ("a",t,s,mempty)

prepareToRetrigger ::
  RDuration -> [ (RDuration,             Msg)]
            -> [((RDuration, RDuration), Msg)]
prepareToRetrigger sup0 dms = f dms where
  endTime = fst (head dms) + sup0

  -- Given an event `(t,m)` and the time `next` of the event one,
  -- create two messages. One sends `m + (trigger=1)` at time `t`,
  -- and the other sends `m + (trigger=(-1))` halfway from `t` to `next`.
  triggerPair :: RTime ->      (RDuration, Msg)
              -> ( ( (RDuration,RDuration), Msg)
                 , ( (RDuration,RDuration), Msg) )
  triggerPair next (t,m) =
    let halfway = (t+next) / 2
    in ( ( (t      , halfway ), M.insert "trigger" 1 m)
       , ( (halfway, next    ), M.insert "trigger" 0 m) )

  f :: [(RDuration, Msg)] -> [((RDuration,RDuration),Msg)]
  f []                      = []
  f [(t,m)]                 = let (a,b) = triggerPair endTime (t,m)
                              in [a,b]
  f ((t0,a0):e@(t1,_):rest) = let (a,b) = triggerPair t1 (t0,a0)
                              in [a,b] ++ f (e:rest)

-- | `hold sup0 tas` sustains each event in `tas` until the next one starts.
-- The `sup0` parameter indicates the total duration of the pattern,
-- so that the last one can wrap around appropriately.
hold :: forall a t. Num t => t -> [(t,a)] -> [((t,t),a)]
hold sup0 tas = _hold tas where
  endTime = fst (head tas) + sup0

  _hold :: [(t,a)] -> [((t,t),a)]
  _hold [] = []
  _hold [(t,a)] =
    [((t,endTime),a)]
  _hold ((t0,a0):(t1,a1):rest) =
    ((t0,t1),a0) : _hold ((t1,a1):rest)

-- | `insertOffs` turns every message off,
-- whether it was on or off before.
insertOffs :: Museq l Msg -> Museq l Msg
insertOffs = vec %~ V.map go where
  go :: Ev l (M.Map String Float)
     -> Ev l (M.Map String Float)
  go = evData %~ M.insert "on" 0

-- | `insertOns` does not change any extant `on` messages,
-- but where they are missing, it inserts `on = 1`.
insertOns :: Museq l Msg -> Museq l Msg
insertOns = vec %~ V.map go where
  go :: Ev l (M.Map String Float)
     -> Ev l (M.Map String Float)
  go = evData %~ M.insertWith (flip const) "on" 1


-- | = Timing a Museq
timeToPlayThrough :: Museq l a -> RTime
timeToPlayThrough m = RTime $ lcmRatios (tr $ _sup m) (tr $ _dur m)

supsToPlayThrough :: Museq l a -> RTime
supsToPlayThrough m = timeToPlayThrough m / (_sup m)

dursToPlayThrough :: Museq l a -> RTime
dursToPlayThrough m = timeToPlayThrough m / (_dur m)

timeToRepeat :: Museq l a -> RTime
timeToRepeat m = let x = timeToPlayThrough m
                  in if x == _dur m then _sup m else x

supsToRepeat :: Museq l a -> RTime
supsToRepeat m = timeToRepeat m / _sup m

dursToRepeat :: Museq l a -> RTime
dursToRepeat m = timeToRepeat m / _dur m

longestDur :: Museq l a -> RDuration
longestDur m = let eventDur ev = (ev ^. evEnd) - (ev ^. evStart)
                in V.maximum $ V.map eventDur $ _vec m


-- | = Naming events

labelsToStrings :: Show l => Museq l a -> Museq String a
labelsToStrings = over vec $ V.map $ over evLabel show

-- | The names in a `Museq Name` are valid if no events with the same
-- name overlap in time, within or across cycles.
museqMaybeNamesAreValid :: forall l a. Eq l
                         => Museq (Maybe l) a -> Bool
museqMaybeNamesAreValid m = and $ map goodGroup nameGroups where
  namedEvents = filter (Mb.isJust . view evLabel) $ V.toList $ _vec m
  nameGroups = L.groupBy ((==) `on` view evLabel) namedEvents
    :: [[Ev (Maybe l) a]]
  adjacentOverlap, wrappedOverlap, goodGroup
    :: [Ev (Maybe l) a] -> Bool
  adjacentOverlap [] = False
  adjacentOverlap (_:[]) = False
  adjacentOverlap (e : f : more) = overlap (e^.evArc) (f^.evArc)
                                   || adjacentOverlap (f:more)
  wrappedOverlap evs = overlap (last evs ^. evArc)
                       (bumpArc (_sup m) $ head evs ^. evArc)
  goodGroup g = not $ adjacentOverlap g || wrappedOverlap g

nameAnonEvents :: forall a.
                   Museq (Maybe String) a
                -> Museq        String  a
nameAnonEvents m =
  sortMuseq $ m {_vec = V.fromList $ namedEvs' ++ namedAnons}
  where
    evs, namedEvs, anonEvs :: [Ev (Maybe String) a]
    evs = V.toList $ _vec m
    (namedEvs, anonEvs) = L.partition (Mb.isJust . view evLabel) evs
    namedEvs' = map (over evLabel Mb.fromJust) namedEvs :: [Ev String a]
    names = map (view evLabel) namedEvs' :: [String]
    anonEvs' = map (over evLabel $ const ()) anonEvs :: [Ev () a]
    intNamedAnons = intNameEvents (_sup m) anonEvs' :: [Ev Int a]
    namedAnons = map (over evLabel f) intNamedAnons :: [Ev String a]
      where f = ((++) $ unusedName names) . show
            -- The ++ ensures no name conflicts.

-- | Assign a minimal number of names (which are integers in string form),
-- starting from 1, so that like-named events do not overlap.
-- ASSUMES the input list is sorted on (start,end) times.
intNameEvents :: RDuration -- ^ _sup of the Museq these Evs come from
               -> [Ev () a] -- ^ these are being named
               -> [Ev Int a]
intNameEvents sup0 (ev1:more) =
  ev1' : _intNameEvents sup0 ev1' [ev1'] more
  where ev1' = over evLabel (const 1) ev1
intNameEvents _ _ = error "intNameEvents: uncaught input pattern."

_intNameEvents :: forall a.
  RDuration -- ^ _sup of the Museq these Evs come from
  -> (Ev Int a) -- ^ the first event
  -> [Ev Int a] -- ^ ongoing events
  -> [Ev ()  a] -- ^ these are being named
  -> [Ev Int a]
_intNameEvents _ _ _ [] = []
_intNameEvents sup0 ev1 ongoing (ev : more) =
  over evLabel (const name) ev
  : _intNameEvents sup0 ev1 ongoing' more
  where
    ongoing' = filter (\ev' -> overlap (ev ^. evArc) (ev' ^. evArc)) ongoing
      -- ongoing in the sense that they do not end before ev starts
    firstOverlaps = overlap (bumpArc sup0 $ ev1 ^. evArc) (ev ^. evArc)
    overlappingMaybeNames = if firstOverlaps
                            then (ev1 ^. evLabel) : ns
                            else                    ns
      where ns = map (view evLabel) ongoing'
    name = head $ (L.\\) [1..] overlappingMaybeNames


-- | = More
-- | Given a Museq, find the synths it uses.
museqSynths :: Museq String Note -> [(SynthDefEnum, SynthName)]
museqSynths m = map f evs where
  evs = V.toList $ _vec m :: [Ev String Note]
  f :: Ev String Note -> (SynthDefEnum, SynthName)
  f ev = ( view (evData . noteSd) ev
         , view evLabel ev )

-- | Given an old set of Museqs and a new one, figure out
-- which synths need to be created, and which destroyed.
-- PITFALL: Both resulting lists are ordered on the first element,
-- likely differing from either of the input maps.
museqsDiff :: M.Map MuseqName (Museq String Note)
            -> M.Map MuseqName (Museq String Note)
            -> ([(SynthDefEnum, SynthName)],
                 [(SynthDefEnum, SynthName)])
museqsDiff old new = (toFree,toCreate) where
  oldMuseqs = M.elems old :: [Museq String Note]
  newMuseqs = M.elems new :: [Museq String Note]
  oldSynths = unique $ concatMap museqSynths oldMuseqs
  newSynths = unique $ concatMap museqSynths newMuseqs
  toCreate = (L.\\) newSynths oldSynths
  toFree = (L.\\) oldSynths newSynths


-- | = Sort a Museq
sortMuseq :: Museq l a -> Museq l a
sortMuseq = vec %~
  \v -> runST $ do v' <- V.thaw v
                   sortBy (compare `on` view evArc) v'
                   V.freeze v'

-- | A valid Museq m is sorted on start and then end times,
-- with all end times >= the corresponding start times,
-- has (relative) duration > 0, and all events at time < _sup m.
-- (todo ? I'm not sure the end-time sort helps.)
-- PITFALL : The end times are permitted to be greater than the _sup.
museqIsValid :: (Eq a, Eq l) => Museq l a -> Bool
museqIsValid mu = and [a,b,c,d,e] where
  v = _vec mu
  a = if V.length v == 0 then True
      else (view evEnd $ V.last v) < _sup mu
  b = mu == sortMuseq mu
  c = _dur mu > 0
  d = _sup mu > 0
  e = V.all (uncurry (<=) . view evArc) v

-- todo ? `arc` could be ~2x faster by using binarySearchRByBounds
-- instead of binarySearchR, to avoid searching the first part
-- of the vector again.
-- | Finds the events in [from,to).
arc :: forall l a. Time -> Duration -> Time -> Time
     -> Museq l a -> [Event Time l a]
arc time0 tempoPeriod from to m =
  let period = tempoPeriod * tr (_sup m) :: Duration
      startVec = V.map (view evStart) $ _vec $ m :: V.Vector RTime
      latestPhase0 = prevPhase0 time0 period from :: Time
        -- it would be natural to start here, but long events from
        -- earlier cycles could carry into now, so we must back up
      earlierFrom = latestPhase0 - tr (longestDur m) * tempoPeriod :: Time
      oldestRelevantCycle = div' (earlierFrom - latestPhase0) period :: Int
      correctAbsoluteTimes :: (Time,Time) -> (Time,Time)
        -- HACK: the inputs ought to be RTimes,
        -- but then I'd be switching types, from Ev to AbsEv
      correctAbsoluteTimes (a,b) = (f a, f b) where
        f rt = tr rt * tempoPeriod + latestPhase0
      chopStarts :: (Time,Time) -> (Time,Time)
      chopStarts = over _1 $ max from
      chopEnds :: (Time,Time) -> (Time,Time)
      chopEnds = over _2 $ min to
      dropImpossibles :: [Event Time l a] -> [Event Time l a]
        -- Because chopStarts can leave an old event starting after it ended.
      dropImpossibles = filter $ uncurry (<=) . view evArc
      futzTimes :: Event RTime l a -> Event Time l a
      futzTimes ev = over evArc f $ eventRTimeToEventTime ev
        where f = chopEnds . chopStarts . correctAbsoluteTimes
   in dropImpossibles $
      map futzTimes
      $ _arcFold oldestRelevantCycle period startVec time0 earlierFrom to m

_arcFold :: forall l a. Int -> Duration -> V.Vector RTime
  -> Time -> Time -> Time -- ^ the same three `Time` arguments as in `arc`
  -> Museq l a -> [Ev l a]
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
             $ V.map (over evEnd   (+(_sup m * fromIntegral cycle)))
             $ V.map (over evStart (+(_sup m * fromIntegral cycle)))
             $ V.slice startIndex (endIndex-startIndex) $ _vec m
       in eventsThisCycle
          ++ _arcFold (cycle+1) period startVec time0 (pp0 + period) to m
