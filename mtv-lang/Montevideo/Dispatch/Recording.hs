{-# LANGUAGE TemplateHaskell #-}

module Montevideo.Dispatch.Recording
where

import           Control.Lens
import           Data.Either.Combinators
import           Data.List.Lens
import           Data.Map (Map)
import qualified Data.Map as M
import qualified Data.Vector as V

import Montevideo.Dispatch.Types.Many
import Montevideo.Dispatch.Types.Time
import Montevideo.Synth
import Montevideo.Synth.Msg
import Montevideo.Synth.Samples


-- | No need to export this.
data ToMuseq label = ToMuseq
  { _tmObservations :: [ Observation ( ScAction label ) ]
    -- ^ Begins as the original list, gets depleted.
  , _tmUnpairedEnds :: Map label ( Observation
                                   ( ScAction label ) )
    -- ^ Unpaired ends. Begins empty, ends empty.
    -- (Actually if there are ends that correspond to no start,
    -- when the algorithm finishes it will still contain those
    -- ends. That doesn't trigger an error.)
  , _tmEvents :: [ Ev label ScParams ] }
    -- ^ Result: Pairs of starts and ends. Begins empty.
  deriving (Show, Eq, Ord)
makeLenses ''ToMuseq

-- | The monome only ever sends New and Free messages.
-- When converting a recording of such messages to a Museq,
-- it is natural to join those corresponding pairs into a single Event.
--
-- Algorithm:
-- Start with an empty map from labels to `ScAction_New`s,
-- and an empty list of events.
-- For note starts that were not ended before the recording,
-- end each at the end of the recording, and add it to the list.
-- For note ends, add them to the map.
-- If a note start was ended before the recording,
-- it will be available in the map.

monomeRecording_toMuseq :: forall l . Ord l =>
  Recording ScAction l -> Either String (Museq l ScParams)
monomeRecording_toMuseq r =
  mapLeft ("monomeRecording_toMuseq: " ++) $ do

  endTime :: Time <- case r ^. recordingEnd of
    Nothing -> Left "Unfinished recording."
    Just t ->  Right t
    -- All times are relative to `endTime`;
    -- the `sup` and `dur` of the `Museq` are 1.

  let obsToRTime :: Observation (ScAction l) -> RTime
      obsToRTime o = RTime $ _unTime $
        (_observationTime o - _recordingStart r)
        / (endTime - _recordingStart r)

      freeAtEnd = Observation
        { _observationData = undefined -- Unneeded, because `pairToEvent`
          -- gets its `_observationData` from the start, not the end.
        , _observationTime = 1 }

      newToFree :: ScAction l -> ScAction l
      newToFree sca = ScAction_Free
        { _actionSynthDefEnum = _actionSynthDefEnum sca
        , _actionSynthName = _actionSynthName sca }

      pairToEvent :: Observation (ScAction l) -> Observation  (ScAction l)
                  -> Ev l ScParams
      pairToEvent start end = Event
        -- Not enforced: `start` and `end` should be equal in label, synthdef.
        { _evLabel = _actionSynthName $ _observationData start
        , _evArc = ( obsToRTime start
                   , obsToRTime end )
        , _evData = _actionScParams $ _observationData start }

      pairsToEvents :: ToMuseq l -> [ Ev l ScParams ]
      pairsToEvents tm = case tm ^. tmObservations of
        [] -> tm ^. tmEvents
          -- `_tmUnpairedEnds` might still not be empty,
          -- but if so, those ends must be discarded,
          -- as there's no record of the parameters (esp. frequency)
          -- of the note they ended.

        (o:os) -> let
          sca :: ScAction l = o ^. observationData
          in case sca of

               ScAction_New _ _ _ ->
                 let l = _actionSynthName sca
                     end :: Observation (ScAction l) =
                       case M.lookup l $ _tmUnpairedEnds tm of
                         Nothing -> freeAtEnd
                         Just e -> e
                 in pairsToEvents $ tm
                    & tmObservations .~ os
                    & tmUnpairedEnds %~ M.delete l
                    & tmEvents %~ (:) (pairToEvent o end)

               ScAction_Free _ _ -> pairsToEvents $ tm
                 & tmObservations .~ os
                 & tmUnpairedEnds %~ M.insert (_actionSynthName sca) o

               ScAction_Send _ _ _ ->
                 -- These are ignored. They don't happen much,
                 -- and using them would require capturing the initial
                 -- (mostly default) synth state.
                 pairsToEvents $ tm { _tmObservations = os }

      events :: [ Ev l ScParams ] = pairsToEvents $ ToMuseq
        { _tmObservations = r ^. recordingData
        , _tmUnpairedEnds = mempty
        , _tmEvents = mempty }

  Right $ Museq { _dur = 1
                , _sup = 1
                , _vec = V.fromList events }
