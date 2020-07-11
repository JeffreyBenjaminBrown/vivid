{-# LANGUAGE DataKinds #-}

module Montevideo.Dispatch.Msg.Mk (
    boopMsg    -- ^ Msg -> [Msg' BoopParams]
  , samplerMsg -- ^ Msg -> [Msg' SamplerParams]
  , sqfmMsg    -- ^ Msg -> [Msg' SqfmParams]
  , vapMsg     -- ^ Msg -> [Msg' VapParams]
  , zotMsg     -- ^ Msg -> [Msg' ZotParams]
  )
where

import qualified Data.Map as M

import Vivid
import Montevideo.Synth
import Montevideo.Dispatch.Types


set' :: VividAction m => Synth params -> Msg' params -> m ()
set' _synth (Msg' m) = set _synth m


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

zotMsg :: Msg -> [Msg' ZotParams]
zotMsg = map zotOneMsg . M.toList


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

zotOneMsg :: (ParamName, Float) -> Msg' ZotParams
zotOneMsg ("on",n)    = Msg' (toI n :: I "on")
zotOneMsg ("amp",n)   = Msg' (toI n :: I "amp")
zotOneMsg ("pulse",n) = Msg' (toI n :: I "pulse")
zotOneMsg ("freq",n)  = Msg' (toI n :: I "freq")
zotOneMsg ("fm-b",n)  = Msg' (toI n :: I "fm-b")
zotOneMsg ("fm-m",n)  = Msg' (toI n :: I "fm-m")
zotOneMsg ("fm-f",n)  = Msg' (toI n :: I "fm-f")
zotOneMsg ("pm-b",n)  = Msg' (toI n :: I "pm-b")
zotOneMsg ("pm-m",n)  = Msg' (toI n :: I "pm-m")
zotOneMsg ("pm-f",n)  = Msg' (toI n :: I "pm-f")
zotOneMsg ("w",n)     = Msg' (toI n :: I "w")
zotOneMsg ("wm-b",n)  = Msg' (toI n :: I "wm-b")
zotOneMsg ("wm-m",n)  = Msg' (toI n :: I "wm-m")
zotOneMsg ("wm-f",n)  = Msg' (toI n :: I "wm-f")
zotOneMsg ("am",n)    = Msg' (toI n :: I "am")
zotOneMsg ("am-b",n)  = Msg' (toI n :: I "am-b")
zotOneMsg ("am-f",n)  = Msg' (toI n :: I "am-f")
zotOneMsg ("rm",n)    = Msg' (toI n :: I "rm")
zotOneMsg ("rm-b",n)  = Msg' (toI n :: I "rm-b")
zotOneMsg ("rm-f",n)  = Msg' (toI n :: I "rm-f")
zotOneMsg ("lpf",n)   = Msg' (toI n :: I "lpf")
zotOneMsg ("lpf-m",n) = Msg' (toI n :: I "lpf-m")
zotOneMsg ("bpf",n)   = Msg' (toI n :: I "bpf")
zotOneMsg ("bpf-m",n) = Msg' (toI n :: I "bpf-m")
zotOneMsg ("bpf-q",n) = Msg' (toI n :: I "bpf-q")
zotOneMsg ("hpf",n)   = Msg' (toI n :: I "hpf")
zotOneMsg ("hpf-m",n) = Msg' (toI n :: I "hpf-m")
zotOneMsg ("lim",n)   = Msg' (toI n :: I "lim")
zotOneMsg ("sh",n)    = Msg' (toI n :: I "sh")
zotOneMsg ("sh-b",n)  = Msg' (toI n :: I "sh-b")
zotOneMsg ("del",n)   = Msg' (toI n :: I "del")
zotOneMsg (param,val) = error $
  "zotOneMsg: unexpected message: " ++ show param ++ "=" ++ show val
