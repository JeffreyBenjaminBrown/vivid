{-# LANGUAGE DataKinds #-}

module Montevideo.Synth.Msg (
    boopScMsg    -- ^ ScMsg -> [ScMsg' BoopParams]
  , samplerScMsg -- ^ ScMsg -> [ScMsg' SamplerParams]
  , sqfmScMsg    -- ^ ScMsg -> [ScMsg' SqfmParams]
  , vapScMsg     -- ^ ScMsg -> [ScMsg' VapParams]
  , zotScMsg     -- ^ ScMsg -> [ScMsg' ZotParams]
  )
where

import qualified Data.Map as M

import Vivid
import Montevideo.Synth
import Montevideo.Dispatch.Types


-- | == per-synth boilerplate

-- | = send a Map full of messages
boopScMsg :: ScMsg -> [ScMsg' BoopParams]
boopScMsg = map boopOneScMsg . M.toList

samplerScMsg :: ScMsg -> [ScMsg' SamplerParams]
samplerScMsg = map samplerOneScMsg . M.toList

sqfmScMsg :: ScMsg -> [ScMsg' SqfmParams]
sqfmScMsg = map sqfmOneScMsg . M.toList

vapScMsg :: ScMsg -> [ScMsg' VapParams]
vapScMsg = map vapOneScMsg . M.toList

zotScMsg :: ScMsg -> [ScMsg' ZotParams]
zotScMsg = map zotOneScMsg . M.toList


-- | = send a message regarding a single parameter

boopOneScMsg :: (ParamName, Float) -> ScMsg' BoopParams
boopOneScMsg ("on",n)    = ScMsg' (toI n :: I "on")
boopOneScMsg ("freq",n)  = ScMsg' (toI n :: I "freq")
boopOneScMsg ("amp",n)   = ScMsg' (toI n :: I "amp")
boopOneScMsg (param,val) = error $
  "boopOneScMsg: unexpected message: " ++ show param ++ "=" ++ show val

samplerOneScMsg :: (ParamName, Float) -> ScMsg' SamplerParams
samplerOneScMsg ("amp",n)     = ScMsg' (toI n :: I "amp")
samplerOneScMsg ("buffer",n)  = ScMsg' (toI n :: I "buffer")
samplerOneScMsg ("speed",n)   = ScMsg' (toI n :: I "speed")
samplerOneScMsg ("trigger",n) = ScMsg' (toI n :: I "trigger")
samplerOneScMsg (param,val)   = error $
  "sqfmOneScMsg: unexpected message: " ++ show param ++ "=" ++ show val

sqfmOneScMsg :: (ParamName, Float) -> ScMsg' SqfmParams
sqfmOneScMsg ("freq",n)           = ScMsg' (toI n :: I "freq")
sqfmOneScMsg ("amp",n)            = ScMsg' (toI n :: I "amp")
sqfmOneScMsg ("width",n)          = ScMsg' (toI n :: I "width")
sqfmOneScMsg ("width-vib-amp",n)  = ScMsg' (toI n :: I "width-vib-amp")
sqfmOneScMsg ("width-vib-freq",n) = ScMsg' (toI n :: I "width-vib-freq")
sqfmOneScMsg (param,val)          = error $
  "sqfmOneScMsg: unexpected message: " ++ show param ++ "=" ++ show val

vapOneScMsg :: (ParamName, Float) -> ScMsg' VapParams
vapOneScMsg ("freq",n)       = ScMsg' (toI n :: I "freq")
vapOneScMsg ("amp",n)        = ScMsg' (toI n :: I "amp")
vapOneScMsg ("saw",n)        = ScMsg' (toI n :: I "saw")
vapOneScMsg ("delay-freq",n) = ScMsg' (toI n :: I "delay-freq")
vapOneScMsg ("delay-amp",n)  = ScMsg' (toI n :: I "delay-amp")
vapOneScMsg ("fm-freq",n)    = ScMsg' (toI n :: I "fm-freq")
vapOneScMsg ("fm-amp",n)     = ScMsg' (toI n :: I "fm-amp")
vapOneScMsg ("fm2-freq",n)   = ScMsg' (toI n :: I "fm2-freq")
vapOneScMsg ("fm2-amp",n)    = ScMsg' (toI n :: I "fm2-amp")
vapOneScMsg ("nz-lpf",n)     = ScMsg' (toI n :: I "nz-lpf")
vapOneScMsg (param,val)      = error $
  "vapOneScMsg: unexpected message: " ++ show param ++ "=" ++ show val

zotOneScMsg :: (ParamName, Float) -> ScMsg' ZotParams
zotOneScMsg ("on",n)    = ScMsg' (toI n :: I "on")
zotOneScMsg ("amp",n)   = ScMsg' (toI n :: I "amp")
zotOneScMsg ("pulse",n) = ScMsg' (toI n :: I "pulse")
zotOneScMsg ("freq",n)  = ScMsg' (toI n :: I "freq")
zotOneScMsg ("fm-b",n)  = ScMsg' (toI n :: I "fm-b")
zotOneScMsg ("fm-m",n)  = ScMsg' (toI n :: I "fm-m")
zotOneScMsg ("fm-f",n)  = ScMsg' (toI n :: I "fm-f")
zotOneScMsg ("pm-b",n)  = ScMsg' (toI n :: I "pm-b")
zotOneScMsg ("pm-m",n)  = ScMsg' (toI n :: I "pm-m")
zotOneScMsg ("pm-f",n)  = ScMsg' (toI n :: I "pm-f")
zotOneScMsg ("w",n)     = ScMsg' (toI n :: I "w")
zotOneScMsg ("wm-b",n)  = ScMsg' (toI n :: I "wm-b")
zotOneScMsg ("wm-m",n)  = ScMsg' (toI n :: I "wm-m")
zotOneScMsg ("wm-f",n)  = ScMsg' (toI n :: I "wm-f")
zotOneScMsg ("am",n)    = ScMsg' (toI n :: I "am")
zotOneScMsg ("am-b",n)  = ScMsg' (toI n :: I "am-b")
zotOneScMsg ("am-f",n)  = ScMsg' (toI n :: I "am-f")
zotOneScMsg ("rm",n)    = ScMsg' (toI n :: I "rm")
zotOneScMsg ("rm-b",n)  = ScMsg' (toI n :: I "rm-b")
zotOneScMsg ("rm-f",n)  = ScMsg' (toI n :: I "rm-f")
zotOneScMsg ("lpf",n)   = ScMsg' (toI n :: I "lpf")
zotOneScMsg ("lpf-m",n) = ScMsg' (toI n :: I "lpf-m")
zotOneScMsg ("bpf",n)   = ScMsg' (toI n :: I "bpf")
zotOneScMsg ("bpf-m",n) = ScMsg' (toI n :: I "bpf-m")
zotOneScMsg ("bpf-q",n) = ScMsg' (toI n :: I "bpf-q")
zotOneScMsg ("hpf",n)   = ScMsg' (toI n :: I "hpf")
zotOneScMsg ("hpf-m",n) = ScMsg' (toI n :: I "hpf-m")
zotOneScMsg ("lim",n)   = ScMsg' (toI n :: I "lim")
zotOneScMsg ("sh",n)    = ScMsg' (toI n :: I "sh")
zotOneScMsg ("sh-b",n)  = ScMsg' (toI n :: I "sh-b")
zotOneScMsg ("del",n)   = ScMsg' (toI n :: I "del")
zotOneScMsg (param,val) = error $
  "zotOneScMsg: unexpected message: " ++ show param ++ "=" ++ show val
