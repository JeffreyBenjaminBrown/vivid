-- | Make a Museq

{-# LANGUAGE ScopedTypeVariables #-}

module Montevideo.Dispatch.Museq.Mk (
  -- | = Make a Museq
    mkMuseqFromEvs   -- ^ RDuration -> [Ev l a]            -> Museq l a
  , mkMuseq          -- ^ RDuration -> [(l,RTime,RTime,a)] -> Museq l a
  , mkMuseqOneScParams -- ^ ScParams  -> Museq String ScParams
  , mkMuseqH  -- ^ forall a l. Ord l
              -- => RDuration -> [(l,RDuration,a)]    -> Museq l a
  , mkMuseqHm -- ^ forall a l. Ord l =>
              -- RDuration -> [(l, RTime, Maybe a)] -> Museq l a
  , mkMuseqHo -- ^ forall a l. Ord l
              -- => RDuration -> [(l,RDuration,ScParams)]  -> Museq l ScParams
  , mkMuseqTrig -- ^ forall l. (Ord l, Show l)
    -- => RDuration -> [(l,RTime,Sample,ScParams)]      -> Museq String Note
  , mkMuseqTrig1 -- ^ RDuration -> [(RTime,Sample)]     -> Museq String Note

  -- | Utilities used by the Museq-making functions
  , hold       -- ^ Num t => t -> [(t,a)] -> [((t,t),a)]
  , insertOns  -- ^ Museq l ScParams                    -> Museq l ScParams
  , separateVoices -- ^ Museq l a -> Map l [Event RTime l a]

  , gaps         -- ^ Museq l a -> [(RTime, RTime)]
  , interiorGaps -- ^ Museq l a -> [(RTime, RTime)]
  , exteriorGaps -- ^ Museq l a -> [(RTime, RTime)]
  ) where

import Prelude hiding (cycle)

import           Control.Lens hiding (to,from)
import qualified Data.List as L
import qualified Data.Map as M
import           Data.Map (Map)
import           Data.Maybe
import qualified Data.Vector as V
import           Data.Vector (Vector)

import Montevideo.Dispatch.Types
import Montevideo.Util
import Montevideo.Synth
import Montevideo.Synth.Msg
import Montevideo.Synth.Samples
import Montevideo.Dispatch.Museq


-- | Like `mkMuseqTrig`, but assuming all messages are trigger=1 messages.
mkMuseqTrig1 :: RDuration -> [(RTime,Sample)] -> Museq String Note
mkMuseqTrig1 sup0 = mkMuseqTrig sup0 . map f where
  f (t,s) = ("a",t,s, M.singleton "trigger" 1)

-- | Make a Museq with sample trigger messages.
-- `mkMuseqTrig` sends any two `ScParams` values to different synths, unless
-- they share the same label *and* the same `Sample`.
-- This is guaranteed by computing new labels `show l ++ show Sample`.

mkMuseqTrig :: forall l. (Ord l, Show l) =>
  RDuration -> [(l,RTime,Sample,ScParams)] -> Museq String Note
mkMuseqTrig sup0 evs0 = let
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

mkMuseqOneScParams :: ScParams -> Museq String ScParams
mkMuseqOneScParams m = mkMuseqH 1 [("a", RTime 0, m)]

-- | Makes a `Museq` using `hold`, holding each `Just` value
-- until the next `Nothing`, then discarding any `Nothing`s.
mkMuseqHm :: forall a l. Ord l
          => RDuration -> [(l, RTime, Maybe a)] -> Museq l a
mkMuseqHm d = f . mkMuseqH d where
  f :: Museq l (Maybe a) -> Museq l a
  f = vec %~ ( V.map unwrap . V.filter test ) where
    test :: Event RTime l (Maybe a) -> Bool
    test = isJust . _evData
    unwrap :: Event RTime l (Maybe a) -> Event RTime l a
    unwrap = fmap $ maybe (error "impossible") id

-- | Like `mkMuseqH`, but inserts `on = 1` in `Event`s that do not
-- mention the `on` parameter. Specialized to `ScParams` payloads.
mkMuseqHo :: forall l. Ord l
          => RDuration -> [(l,RDuration,ScParams)] -> Museq l ScParams
mkMuseqHo d evs0 = insertOns $ mkMuseqH d evs0

-- | Makes a `Museq` using `hold`,
-- so that each event lasts until the next.
mkMuseqH :: forall a l. Ord l
          => RDuration -> [(l,RDuration,a)] -> Museq l a
mkMuseqH d = mkMuseq_seqProc (hold d) d

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

separateVoices :: forall l a. Ord l
               => Museq l a -> Map l [Event RTime l a]
separateVoices m = let
  f :: Map l [Event RTime l a]
    ->        Event RTime l a
    -> Map l [Event RTime l a]
  f m e = case M.lookup     (_evLabel e)     m of
    Nothing -> M.insert     (_evLabel e) [e] m
    Just l -> M.adjust (e:) (_evLabel e)     m
  in V.foldl f mempty $ V.reverse $ _vec m

-- | `insertOffs` adds new events with a single `on=0` message,
-- for every stretch in which a voice is inactive.
insertOffs :: forall a l. Ord l
                 => Museq l a -> Museq l a
insertOffs m = let
  voices :: [[Ev l a]] =
    M.elems $ separateVoices m

  -- TODO ? maybe use these function
  -- `boundaries`
  -- `partitionAndGroupEventsAtBoundaries`
  -- `partitionArcAtTimes`

  doVoice :: [Ev l m] -> [Ev l m]
  doVoice es = undefined

  in undefined

gaps :: forall l a. (Eq a, Ord a)
     => Museq l a -> [(RTime, RTime)]
gaps m = L.sort $ interiorGaps m ++ exteriorGaps m

-- *** Algorithm:
--     Suppose the events are ordered by (start,end).
--     Let (si,ei) denote the ith event.
--     Begin with the "latest interval" equal to (s1,e1).
--     If s2 =< e1, replace e1 with the greater of e1 and e2.
--     Otherwise s2 > e1, and (e1,s2) represents a gap.
--     Add it to the list of gaps,
--     and redefine "latest interval" as (s2,e2).
--     Repeat.

data GapCalc = GapCalc -- ^ No need to export this.
  { _latestInterval :: (RTime,RTime)
  , _gaps :: [ (RTime,RTime) ] }

interiorGaps :: forall l a. (Eq a, Ord a)
             => Museq l a -> [(RTime, RTime)]
interiorGaps m =
  let ((s1,e1) : is) :: [(RTime, RTime)] =
        map _evArc $ V.toList $ _vec m
      go :: GapCalc -> (RTime,RTime) -> GapCalc
      go gc (sk,ek) = let
        (sl,el) = _latestInterval gc
        in if sk > el
           then GapCalc { _latestInterval = (sk,ek)
                        , _gaps = (el,sk) : _gaps gc }
           else gc { _latestInterval = (sl, max el ek) }
      gc1 = GapCalc { _latestInterval = (s1,e1)
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

exteriorGaps :: forall l a. (Eq a, Ord a)
        => Museq l a -> [(RTime, RTime)]
exteriorGaps m =
  let is @ ((s1,_) : _) :: [(RTime, RTime)] =
        map _evArc $ V.toList $ _vec m
      ek :: RTime = maximum $ map snd is
      end = _sup m
  in if ek < end
     then [(0,s1), (ek,end)]
     else if ek - end < s1
          then [(ek - end, s1)]
          else []
