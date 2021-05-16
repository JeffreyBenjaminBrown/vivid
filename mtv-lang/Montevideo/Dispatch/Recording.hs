module Montevideo.Dispatch.Recording (
  monomeRecording_toMuseq
  ) where

import           Control.Lens
import           Data.Either.Combinators
import           Data.Map (Map)
import qualified Data.Map as M

import Montevideo.Dispatch.Types.Many
import Montevideo.Dispatch.Types.Time
import Montevideo.Synth
import Montevideo.Synth.Msg
import Montevideo.Synth.Samples


-- | No need to export this.
type ToMuseq label =
  ( [ Observation ( ScAction label ) ]
    -- ^ Begins as the original list, then depleted.
  , Map label (ScAction label)
    -- ^ Unpaired ends. Begins empty, ends empty.
  , [ Event Time label Note ] )
    -- ^ Result: Pairs of starts and ends. Begins empty.

-- | The monome only ever sends New and Free messages.
-- When converting a recording of such messages to a Museq,
-- it is natural to join those corresponding pairs into a single Event.

-- Algorithm:
-- Start with an empty map from labels to `ScAction_New`s,
-- and an empty list of events.
-- For note starts that were not ended before the recording,
-- end each at the end of the recording, and add it to the list.
-- For note ends, add them to the map.
-- If a note start was ended before the recording,
-- it will be available in the map.

monomeRecording_toMuseq :: Recording ScAction label
                        -> Either String (Museq label ScAction)
monomeRecording_toMuseq r = mapLeft ("monomeRecording_toMuseq: " ++) $ do
  if null $ r ^. recordingEnd
    then Left "Unfinished recording."
    else Right ()
  let s0 :: ToMuseq = (mempty, mempty, mempty)
      pairSearch :: (
