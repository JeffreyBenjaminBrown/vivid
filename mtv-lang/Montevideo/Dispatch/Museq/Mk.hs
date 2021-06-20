-- | Make a Museq

{-# LANGUAGE ScopedTypeVariables #-}

module Montevideo.Dispatch.Museq.Mk (
  -- | = Primitives for making a Museq
    mkMuseqFromEvs   -- ^ RDuration -> [Ev l a]            -> Museq l a
  , mkMuseq          -- ^ RDuration -> [(l,RTime,RTime,a)] -> Museq l a
  , mkMuseqOneScParams -- ^ ScParams  -> Museq String ScParams
  , mkMuseqHold  -- ^ forall a l. Ord l
              -- => RDuration -> [(l,RDuration,a)]    -> Museq l a
  , mkMuseq_holdMaybe -- ^ forall a l. Ord l =>
              -- RDuration -> [(l, RTime, Maybe a)] -> Museq l a
  , mkMuseqTrigger -- ^ forall l. (Ord l, Show l)
    -- => RDuration -> [(l,RTime,Sample,ScParams)]      -> Museq String Note
  , mkMuseqTrigger1 -- ^ RDuration -> [(RTime,Sample)]     -> Museq String Note
  , insertOns  -- ^ Museq l ScParams -> Museq l ScParams
  , insertOffs -- ^ Museq l ScParams -> Museq l ScParams

  -- | = Utilities used by the above Museq-making functions.
  -- Probably not worth learning as a user.
  , hold       -- ^ Num t => t -> [(t,a)] -> [((t,t),a)]
  , separateVoices -- ^ Museq l a -> Map l [Event RTime l a]
  , gaps         -- ^ RTime -> [(RTime, RTime)] -> [(RTime, RTime)]
  , interiorGaps -- ^          [(RTime, RTime)] -> [(RTime, RTime)]
  , exteriorGaps -- ^ RTime -> [(RTime, RTime)] -> [(RTime, RTime)]
  ) where

import Prelude hiding (cycle)

import           Control.Lens hiding (to,from)
import qualified Data.List as L
import qualified Data.Map as M
import           Data.Map (Map)
import           Data.Maybe
import           Data.Ord (comparing)
import qualified Data.Vector as V
import           Data.Vector (Vector)

import Montevideo.Dispatch.Types
import Montevideo.Util
import Montevideo.Synth
import Montevideo.Synth.Msg
import Montevideo.Synth.Samples
import Montevideo.Dispatch.Museq


-- | Like `mkMuseqTrigger`, but assuming all messages are trigger=1 messages.
mkMuseqTrigger1 :: RDuration -> [(RTime,Sample)] -> Museq String Note
mkMuseqTrigger1 sup0 = mkMuseqTrigger sup0 . map f where
  f (t,s) = ("a",t,s, M.singleton "trigger" 1)

-- | Make a Museq with sample trigger messages.
-- `mkMuseqTrigger` sends any two `ScParams` values to different synths, unless
-- they share the same label *and* the same `Sample`.
-- This is guaranteed by computing new labels `show l ++ show Sample`.

mkMuseqTrigger :: forall l. (Ord l, Show l) =>
  RDuration -> [(l,RTime,Sample,ScParams)] -> Museq String Note
mkMuseqTrigger sup0 evs0 = let
  -- todo ? Rather than group by l and then Sample,
  -- maybe group by l' = show l ++ show Sample?
  evs1 :: [ ( (l, Sample)
            , (RTime, ScParams) ) ] =
    map (\(l,t,n,m) -> ((l,n),(t,m))) evs0
  evs2 :: [ ( (l, Sample)
            , [(RTime, ScParams)] )] =
    multiPartition evs1
  evs3 :: [( (l, Sample)
           , [((RTime, RTime) ,ScParams)] )] =
    map (_2 . traversed . _1 %~ (\t -> (t,t)) ) evs2
  evs4 :: [( (l, Sample),
             [( (RTime, RTime), Note )] )] =
    map f evs3
    where f :: ( (l,Sample), [((RTime,RTime), ScParams )] )
            -> ( (l,Sample), [((RTime,RTime), Note)] )
          f ((l,s),rtimeScParamsPairs) =
            ((l,s), map (_2 %~ Note (Sampler s)) rtimeScParamsPairs)

  evs5 :: [( String, [((RTime,RTime), Note)] )] =
    map (_1 %~ \(l,s) -> show l ++ show s) evs4
  evs6 :: [Event RTime String Note] =
    concatMap (\(s,ps) -> map (\(ts,n) -> Event s ts n) ps) evs5
  almost :: Museq String Note =
    mkMuseqFromEvs sup0 evs6
  in almost
  -- g :: (RTime,RTime) -> (RTime,RTime)
  -- g (s,e) = if s >= sup0 then (s-sup0,e-sup0) else (s,e)
  -- in almost & vec %~ V.map (evArc %~ g)
    -- I'm pretty sure this is obsolete.
    -- It ensures that any event starting after sup0 wraps around to the
    -- beginning of the loop. It was probably needed when I was adding
    -- "trigger=0" messages to the Museq. Now the Dispatch handles that;
    -- the user never needs to see those messages.

mkMuseq :: RDuration -> [(l,RTime,RTime,a)] -> Museq l a
mkMuseq d = mkMuseqFromEvs d . map f where
  f (l,start,end,a) = Event l (start,end) a

-- | A Museq with a single constant state.
mkMuseqOneScParams :: ScParams -> Museq String ScParams
mkMuseqOneScParams m = mkMuseq 1 [("a", RTime 0, RTime 1, m)]

-- | Makes a `Museq` using `hold`, holding each `Just` value
-- until the next `Nothing`, then discarding any `Nothing`s.
mkMuseq_holdMaybe :: forall a l. Ord l
          => RDuration -> [(l, RTime, Maybe a)] -> Museq l a
mkMuseq_holdMaybe d = f . mkMuseqHold d where
  f :: Museq l (Maybe a) -> Museq l a
  f = vec %~ ( V.map unwrap . V.filter test ) where
    test :: Event RTime l (Maybe a) -> Bool
    test = isJust . _evData
    unwrap :: Event RTime l (Maybe a) -> Event RTime l a
    unwrap = fmap $ maybe (error "impossible") id

-- | Makes a `Museq` using `hold`,
-- so that each event lasts until the next.
mkMuseqHold :: forall a l. Ord l
          => RDuration -> [(l,RDuration,a)] -> Museq l a
mkMuseqHold d = mkMuseq_seqProc (hold d) d

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
  in mkMuseqFromEvs d evs4

mkMuseqFromEvs :: RDuration -> [Ev l a] -> Museq l a
mkMuseqFromEvs d evs =
  sortMuseq $ Museq { _dur = d
                    , _sup = d
                    , _vec = V.fromList $ evs }


-- | Utilities used by the Museq-making functions

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

-- | `insertOns` does not change any extant `on` messages,
-- but where they are missing, it inserts `on = 1`.
insertOns :: Museq l ScParams -> Museq l ScParams
insertOns = vec %~ V.map go where
  go :: Ev l (M.Map String Float)
     -> Ev l (M.Map String Float)
  go = evData %~ M.insertWith (flip const) "on" 1

-- | `insertOffs` adds new events with a single `on=0` message,
-- for every stretch in which a voice is inactive.
insertOffs :: forall l. Ord l
           => Museq l ScParams -> Museq l ScParams
insertOffs m = let
  voices :: Map l [Ev l ScParams] =
    -- TODO ? just keep the map elements (M.elems),
    -- since the labels are already part of every Event.
    separateVoices m

  -- PITFALL: The output of `addGapsToVoice` is unsorted.
  addGapsToVoice :: l -> [Ev l ScParams] -> [Ev l ScParams]
  addGapsToVoice l es =
    let
      voiceGapArcs :: [Ev l a] -> [(RTime,RTime)]
      voiceGapArcs es = gaps (_sup m) $ map _evArc es
      gapArc_toOff :: l -> (RTime,RTime) -> Ev l ScParams
      gapArc_toOff l (s,e) = Event l (s,e) $ M.singleton "on" 0
      offs :: [Ev l ScParams] =
        map (gapArc_toOff l) $ voiceGapArcs es
    in es ++ offs

  in m { _vec = V.fromList
                $ L.sortBy (comparing _evArc)
                $ concat $ M.elems
                $ M.mapWithKey addGapsToVoice voices }

separateVoices :: forall l a. Ord l
               => Museq l a -> Map l [Event RTime l a]
separateVoices m = let
  f :: Map l [Event RTime l a]
    ->        Event RTime l a
    -> Map l [Event RTime l a]
  f m e = case M.lookup      (_evLabel e)     m of
    Nothing -> M.insert      (_evLabel e) [e] m
    Just l  -> M.adjust (e:) (_evLabel e)     m
  in V.foldl f mempty $ V.reverse $ _vec m

gaps :: RTime -> [(RTime, RTime)] -> [(RTime, RTime)]
gaps sup0 m = L.sort $ interiorGaps m ++ exteriorGaps sup0 m

-- *** Algorithm:
--     Suppose the events are ordered by (start,end).
--     Let (si,ei) denote the ith event.
--     Begin with the "latest arc" equal to (s1,e1).
--     If s2 =< e1, replace e1 with the greater of e1 and e2.
--     Otherwise s2 > e1, and (e1,s2) represents a gap.
--     Add it to the list of gaps,
--     and redefine "latest arc" as (s2,e2).
--     Repeat.

data GapCalc = GapCalc -- ^ No need to export this.
  { _latestArc :: (RTime,RTime)
  , _gaps :: [ (RTime,RTime) ] }

interiorGaps :: [(RTime, RTime)] -> [(RTime, RTime)]
interiorGaps ((s1,e1) : is) =
  let go :: GapCalc -> (RTime,RTime) -> GapCalc
      go gc (sk,ek) = let
        (sl,el) = _latestArc gc
        in if sk > el
           then GapCalc { _latestArc = (sk,ek)
                        , _gaps = (el,sk) : _gaps gc }
           else gc { _latestArc = (sl, max el ek) }
      gc1 = GapCalc { _latestArc = (s1,e1)
                    , _gaps = [] }
  in reverse $ _gaps $ foldl go gc1 is

-- *** Algorithm
--     Suppose the events are ordered by (start,end).
--     Let (si,ei) denote the ith event.
--     Keep special track of s1.
--     Let (sk,ek) be the event with the greatest end.
--     (Note that sk is necessarily less than _sup.)
--     If ek < _sup, then (0,s1) and (ek,_sup) are gaps.
--     If ek >= _sup and ek - _sup < s1,
--     then (ek - _sup, s1) is a gap.
--     Otherwise there are no gaps on either end.

exteriorGaps :: RTime -> [(RTime, RTime)] -> [(RTime, RTime)]
exteriorGaps sup0 is @ ((s1,_) : _) =
  let ek :: RTime = maximum $ map snd is
  in if ek < sup0
     then [(0,s1), (ek,sup0)]
     else if ek - sup0 < s1
          then [(ek - sup0, s1)]
          else []
