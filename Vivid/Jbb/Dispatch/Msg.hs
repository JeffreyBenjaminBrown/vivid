{-# LANGUAGE DataKinds #-}

module Vivid.Jbb.Dispatch.Msg (
  set'

  , boopMsg
  , sqfmMsg
  , vapMsg

  , boopMapMsg
  , sqfmMapMsg
  , vapMapMsg
  )
where

import qualified Data.Map as M
import Data.List (partition)
import Control.Lens (_1,_2,over)

import Vivid
import Vivid.Jbb.Util
import Vivid.Jbb.Synths
import Vivid.Jbb.Dispatch.Types


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

-- | = MapMsg versions
boopMapMsg :: MapMsg -> [Msg' BoopParams]
boopMapMsg = map boopMsg . M.toList

sqfmMapMsg :: MapMsg -> [Msg' SqfmParams]
sqfmMapMsg = map sqfmMsg . M.toList

vapMapMsg :: MapMsg -> [Msg' VapParams]
vapMapMsg = map vapMsg . M.toList
