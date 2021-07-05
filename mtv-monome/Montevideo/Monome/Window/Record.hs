-- | A single-button window to start and stop recording.

module Montevideo.Monome.Window.Record (
  recordWindow
  ) where

import           Prelude hiding (pred)
import           Control.Lens hiding (Choice)
import           Vivid (getTime)

import           Montevideo.Dispatch.Types
import           Montevideo.Dispatch.Recording
import           Montevideo.Monome.Types.Most


recordWindow :: Window app
recordWindow = Window
  { windowLabel = RecordWindow
  , windowContains = (==) (0,0)
  , windowInitLeds = \_ _ -> Right []
  , windowHandler = handler }

handler :: forall app.
           St app -> (MonomeId, ((X,Y), Switch))
        -> IO (Either String (St app))
handler st (_, (_, False)) =
  return $ Right st
handler st (_, (_, True)) =
  case _stIsRecording st of
    False -> do -- start recording
      nr <- newRecording
      return $ Right $ st
        & stIsRecording .~ True
        & stRecordings %~ (nr :)
    True -> do -- stop recording
      now <- unTimestamp <$> getTime
      return $ Right $ st
        & stIsRecording .~ False
        & stRecordings . _head . recordingEnd .~ Just now
