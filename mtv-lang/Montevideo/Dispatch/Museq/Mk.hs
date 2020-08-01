-- | Make a Museq

{-# LANGUAGE ScopedTypeVariables #-}

module Montevideo.Dispatch.Museq.Mk (
  -- | = Make a Museq
    mkMuseqFromEvs   -- ^ RDuration -> [Ev l a]            -> Museq l a
  , mkMuseq          -- ^ RDuration -> [(l,RTime,RTime,a)] -> Museq l a
  , mkMuseqOneScMsg -- ^          ScMsg                   -> Museq String ScMsg
  , mkMuseqH  -- ^ forall a l. Ord l
              -- => RDuration -> [(l,RDuration,a)]    -> Museq l a
  , mkMuseqHm -- ^ forall a l. Ord l =>
              -- RDuration -> [(l, RTime, Maybe a)] -> Museq l a
  , mkMuseqHo -- ^ forall a l. Ord l
              -- => RDuration -> [(l,RDuration,ScMsg)]  -> Museq l ScMsg
  , mkMuseqTrig -- ^ forall l. (Ord l, Show l)
    -- => RDuration -> [(l,RTime,Sample,ScMsg)]         -> Museq String Note
  , mkMuseqTrig1 -- ^ RDuration -> [(RTime,Sample)]     -> Museq String Note

  -- | Utilities used by the Museq-making functions
  , hold       -- ^ Num t => t -> [(t,a)] -> [((t,t),a)]
  , insertOffs -- ^ Museq l ScMsg                       -> Museq l ScMsg
  , insertOns  -- ^ Museq l ScMsg                       -> Museq l ScMsg
  ) where

import Prelude hiding (cycle)

import Control.Lens hiding (to,from)
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Vector as V

import Montevideo.Dispatch.Types
import Montevideo.Util
import Montevideo.Synth
import Montevideo.Synth.Samples
import Montevideo.Dispatch.Museq


mkMuseqFromEvs :: RDuration -> [Ev l a] -> Museq l a
mkMuseqFromEvs d evs =
  sortMuseq $ Museq { _dur = d
                    , _sup = d
                    , _vec = V.fromList $ evs }

mkMuseq :: RDuration -> [(l,RTime,RTime,a)] -> Museq l a
mkMuseq d = mkMuseqFromEvs d . map f where
  f (l,start,end,a) = Event l (start,end) a

mkMuseqOneScMsg :: ScMsg -> Museq String ScMsg
mkMuseqOneScMsg m = mkMuseqH 1 [("a", RTime 0, m)]

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

-- | Makes a `Museq` using `hold`,
-- so that each event lasts until the next.
mkMuseqH :: forall a l. Ord l
          => RDuration -> [(l,RDuration,a)] -> Museq l a
mkMuseqH d = mkMuseq_seqProc (hold d) d

-- | Makes a `Museq` using `hold`, holding each `Just` value
-- until the next `Maybe`, then discarding any `Nothing`s.
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
-- mention the `on` parameter. Specialized to `ScMsg` payloads.
mkMuseqHo :: forall l. Ord l
          => RDuration -> [(l,RDuration,ScMsg)] -> Museq l ScMsg
mkMuseqHo d evs0 = insertOns $ mkMuseqH d evs0

-- | Make a Museq with sample trigger messages.
-- `mkMuseqTrig` sends any two `ScMsg` values to different synths, unless
-- they share the same label *and* the same `Sample`.
-- This is guaranteed by computing new labels `show l ++ show Sample`.

mkMuseqTrig :: forall l. (Ord l, Show l) =>
  RDuration -> [(l,RTime,Sample,ScMsg)] -> Museq String Note
mkMuseqTrig sup0 evs0 = let
  -- Rather than group by l and then Sample,
  -- maybe group by l' = show l ++ show Sample?
  evs1 :: [ ( (l, Sample)
            , (RTime,ScMsg) ) ] =
    map (\(l,t,n,m) -> ((l,n),(t,m))) evs0
  evs2 :: [( (l, Sample)
           , [(RTime,ScMsg)] )] =
    multiPartition evs1
  evs3 :: [( (l, Sample)
           , [((RTime, RTime) ,ScMsg)] )] =
    map (_2 . traversed . _1 %~ (\t -> (t,t)) ) evs2
  evs4 :: [( (l,Sample),
             [((RTime,RTime), Note)] )] =
    map f evs3
    where f :: ( (l,Sample), [((RTime,RTime), ScMsg )] )
            -> ( (l,Sample), [((RTime,RTime), Note)] )
          f ((l,s),rtimeScMsgPairs) =
            ((l,s), map (_2 %~ Note (Sampler s)) rtimeScMsgPairs)

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

-- | Like `mkMuseqTrig`, but assuming all messages are trigger=1 messages.
mkMuseqTrig1 :: RDuration -> [(RTime,Sample)] -> Museq String Note
mkMuseqTrig1 sup0 = mkMuseqTrig sup0 . map f where
  f (t,s) = ("a",t,s, M.singleton "trigger" 1)


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

-- | `insertOffs` turns every message off,
-- whether it was on or off before.
insertOffs :: Museq l ScMsg -> Museq l ScMsg
insertOffs = vec %~ V.map go where
  go :: Ev l (M.Map String Float)
     -> Ev l (M.Map String Float)
  go = evData %~ M.insert "on" 0

-- | `insertOns` does not change any extant `on` messages,
-- but where they are missing, it inserts `on = 1`.
insertOns :: Museq l ScMsg -> Museq l ScMsg
insertOns = vec %~ V.map go where
  go :: Ev l (M.Map String Float)
     -> Ev l (M.Map String Float)
  go = evData %~ M.insertWith (flip const) "on" 1
