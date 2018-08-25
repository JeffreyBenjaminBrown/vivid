{-# LANGUAGE DataKinds #-}

module Vivid.Jbb.Dispatch.Msg (
  set'

  , boopMsg
  , sqfmMsg
  , vapMsg
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


-- | == per-synth boilerplate

-- | = send a Map full of messages
boopMsg :: MapMsg -> [Msg' BoopParams]
boopMsg = map boopOneMsg . M.toList

sqfmMsg :: MapMsg -> [Msg' SqfmParams]
sqfmMsg = map sqfmOneMsg . M.toList

vapMsg :: MapMsg -> [Msg' VapParams]
vapMsg = map vapOneMsg . M.toList


-- | = send a message regarding a single parameter

boopOneMsg :: Msg -> Msg' BoopParams
boopOneMsg ("freq",n) = Msg' (toI n :: I "freq")
boopOneMsg ("amp",n) = Msg' (toI n :: I "amp")

sqfmOneMsg :: Msg -> Msg' SqfmParams
sqfmOneMsg ("freq",n) = Msg' (toI n :: I "freq")
sqfmOneMsg ("amp",n) = Msg' (toI n :: I "amp")
sqfmOneMsg ("width",n) = Msg' (toI n :: I "width")
sqfmOneMsg ("width-vib-amp",n) = Msg' (toI n :: I "width-vib-amp")
sqfmOneMsg ("width-vib-freq",n) = Msg' (toI n :: I "width-vib-freq")

vapOneMsg :: Msg -> Msg' VapParams
vapOneMsg ("freq",n) = Msg' (toI n :: I "freq")
vapOneMsg ("amp",n) = Msg' (toI n :: I "amp")
vapOneMsg ("saw",n) = Msg' (toI n :: I "saw")
vapOneMsg ("delay-freq",n) = Msg' (toI n :: I "delay-freq")
vapOneMsg ("delay-amp",n) = Msg' (toI n :: I "delay-amp")
vapOneMsg ("fm-freq",n) = Msg' (toI n :: I "fm-freq")
vapOneMsg ("fm-amp",n) = Msg' (toI n :: I "fm-amp")
vapOneMsg ("fm2-freq",n) = Msg' (toI n :: I "fm2-freq")
vapOneMsg ("fm2-amp",n) = Msg' (toI n :: I "fm2-amp")
vapOneMsg ("nz-lpf",n) = Msg' (toI n :: I "nz-lpf")
