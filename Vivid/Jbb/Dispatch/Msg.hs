{-# LANGUAGE DataKinds #-}

module Vivid.Jbb.Dispatch.Msg (
  set'
  , scheduleSend

  , boopMsg
  , sqfmMsg
  , vapMsg
  ) where

import qualified Data.Map as M
import Data.List (partition)
import Control.Lens (_1,_2,over)

import Vivid
import Vivid.Jbb.Util
import Vivid.Jbb.Synths
import Vivid.Jbb.Dispatch.Types


set' :: VividAction m => Synth params -> Msg' params -> m ()
set' synth (Msg' m) = set synth m

scheduleSend :: SynthRegister3 -> (Time,Action) -> IO ()
  -- per-synth boilerplate
scheduleSend reg (t, Send Boop name msg) =
  maybe err success (M.lookup name $ boops3 reg)
  where err = writeTimeAndError $ "No Boop is named " ++ name ++ "."
        success s = doScheduledAt (Timestamp t) $ set' s $ boopMsg msg
scheduleSend reg (t, Send Sqfm name msg) =
  maybe err success (M.lookup name $ sqfms3 reg)
  where err = writeTimeAndError $ "No Sqfm is named " ++ name ++ "."
        success s = doScheduledAt (Timestamp t) $ set' s $ sqfmMsg msg
scheduleSend reg (t, Send Vap name msg) =
  maybe err success (M.lookup name $ vaps3 reg)
  where err = writeTimeAndError $ "No Vap is named " ++ name ++ "."
        success s = doScheduledAt (Timestamp t) $ set' s $ vapMsg msg


-- | = per-synth boilerplate

boopMsg :: Msg -> Msg' BoopParams
boopMsg ("freq",n) = Msg' (toI n :: I "freq")
boopMsg ("amp",n) = Msg' (toI n :: I "amp")

sqfmMsg :: Msg -> Msg' SqfmParams
sqfmMsg ("freq",n) = Msg' (toI n :: I "freq")
sqfmMsg ("amp",n) = Msg' (toI n :: I "amp")
sqfmMsg ("width",n) = Msg' (toI n :: I "width")
sqfmMsg ("width-vib-amp",n) = Msg' (toI n :: I "width-vib-amp")
sqfmMsg ("width-vib-freq",n) = Msg' (toI n :: I "width-vib-freq")

vapMsg :: Msg -> Msg' VapParams
vapMsg ("freq",n) = Msg' (toI n :: I "freq")
vapMsg ("amp",n) = Msg' (toI n :: I "amp")
vapMsg ("saw",n) = Msg' (toI n :: I "saw")
vapMsg ("delay-freq",n) = Msg' (toI n :: I "delay-freq")
vapMsg ("delay-amp",n) = Msg' (toI n :: I "delay-amp")
vapMsg ("fm-freq",n) = Msg' (toI n :: I "fm-freq")
vapMsg ("fm-amp",n) = Msg' (toI n :: I "fm-amp")
vapMsg ("fm2-freq",n) = Msg' (toI n :: I "fm2-freq")
vapMsg ("fm2-amp",n) = Msg' (toI n :: I "fm2-amp")
vapMsg ("nz-lpf",n) = Msg' (toI n :: I "nz-lpf")
