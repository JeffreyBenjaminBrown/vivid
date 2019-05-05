{-# LANGUAGE DataKinds #-}

module Dispatch.Msg (
    set'    -- ^ VividAction m => Synth params -> Msg' params -> m ()
  , boopMsg    -- ^ Msg -> [Msg' BoopParams]
  , samplerMsg -- ^ Msg -> [Msg' SamplerParams]
  , sqfmMsg    -- ^ Msg -> [Msg' SqfmParams]
  , vapMsg     -- ^ Msg -> [Msg' VapParams]
  )
where

import qualified Data.Map as M

import Vivid hiding (synth)
import Synths
import Dispatch.Types


set' :: VividAction m => Synth params -> Msg' params -> m ()
set' synth (Msg' m) = set synth m


-- | == per-synth boilerplate

-- | = send a Map full of messages
boopMsg :: Msg -> [Msg' BoopParams]
boopMsg = map boopOneMsg . M.toList

samplerMsg :: Msg -> [Msg' SamplerParams]
samplerMsg = map samplerOneMsg . M.toList

sqfmMsg :: Msg -> [Msg' SqfmParams]
sqfmMsg = map sqfmOneMsg . M.toList

vapMsg :: Msg -> [Msg' VapParams]
vapMsg = map vapOneMsg . M.toList


-- | = send a message regarding a single parameter

boopOneMsg :: (ParamName, Float) -> Msg' BoopParams
boopOneMsg ("on",n)    = Msg' (toI n :: I "on")
boopOneMsg ("freq",n)  = Msg' (toI n :: I "freq")
boopOneMsg ("amp",n)   = Msg' (toI n :: I "amp")
boopOneMsg (param,val) = error $
  "boopOneMsg: unexpected message: " ++ show param ++ "=" ++ show val

samplerOneMsg :: (ParamName, Float) -> Msg' SamplerParams
samplerOneMsg ("amp",n)     = Msg' (toI n :: I "amp")
samplerOneMsg ("buffer",n)  = Msg' (toI n :: I "buffer")
samplerOneMsg ("speed",n)   = Msg' (toI n :: I "speed")
samplerOneMsg ("trigger",n) = Msg' (toI n :: I "trigger")
samplerOneMsg (param,val)   = error $
  "sqfmOneMsg: unexpected message: " ++ show param ++ "=" ++ show val

sqfmOneMsg :: (ParamName, Float) -> Msg' SqfmParams
sqfmOneMsg ("freq",n)           = Msg' (toI n :: I "freq")
sqfmOneMsg ("amp",n)            = Msg' (toI n :: I "amp")
sqfmOneMsg ("width",n)          = Msg' (toI n :: I "width")
sqfmOneMsg ("width-vib-amp",n)  = Msg' (toI n :: I "width-vib-amp")
sqfmOneMsg ("width-vib-freq",n) = Msg' (toI n :: I "width-vib-freq")
sqfmOneMsg (param,val)          = error $
  "sqfmOneMsg: unexpected message: " ++ show param ++ "=" ++ show val

vapOneMsg :: (ParamName, Float) -> Msg' VapParams
vapOneMsg ("freq",n)       = Msg' (toI n :: I "freq")
vapOneMsg ("amp",n)        = Msg' (toI n :: I "amp")
vapOneMsg ("saw",n)        = Msg' (toI n :: I "saw")
vapOneMsg ("delay-freq",n) = Msg' (toI n :: I "delay-freq")
vapOneMsg ("delay-amp",n)  = Msg' (toI n :: I "delay-amp")
vapOneMsg ("fm-freq",n)    = Msg' (toI n :: I "fm-freq")
vapOneMsg ("fm-amp",n)     = Msg' (toI n :: I "fm-amp")
vapOneMsg ("fm2-freq",n)   = Msg' (toI n :: I "fm2-freq")
vapOneMsg ("fm2-amp",n)    = Msg' (toI n :: I "fm2-amp")
vapOneMsg ("nz-lpf",n)     = Msg' (toI n :: I "nz-lpf")
vapOneMsg (param,val)      = error $
  "vapOneMsg: unexpected message: " ++ show param ++ "=" ++ show val
