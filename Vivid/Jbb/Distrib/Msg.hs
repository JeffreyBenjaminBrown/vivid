{-# LANGUAGE DataKinds #-}

module Vivid.Jbb.Distrib.Msg (
  set'
  , boopMsg
  , sqfmMsg
  , vapMsg
  ) where

import Vivid
import Vivid.Jbb.Synths
import Vivid.Jbb.Distrib.Types


set' :: VividAction m => Synth params -> Msg' params -> m ()
set' synth (Msg' m) = set synth m


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
