{-# LANGUAGE DataKinds
, TemplateHaskell
, GADTs #-}

module Montevideo.Synth.Msg (
    set' -- ^ V.VividAction m => V.Synth params -> ScParams' params -> m ()
  , ParamName, ScParams, ScParams'(..)
  , ScAction(..), _ScAction_New, _ScAction_Send, _ScAction_Free
  , boopScParams    -- ^ ScParams -> [ScParams' BoopParams]
  , moopScParams    -- ^ ScParams -> [ScParams' BoopParams]
  , samplerScParams -- ^ ScParams -> [ScParams' SamplerParams]
  , sqfmScParams    -- ^ ScParams -> [ScParams' SqfmParams]
  , vapScParams     -- ^ ScParams -> [ScParams' VapParams]
  , zotScParams     -- ^ ScParams -> [ScParams' ZotParams]
  )
where

import           Control.Lens (makePrisms)
import           Data.Map (Map)
import qualified Data.Map as M
import qualified Vivid as V

import Vivid
import Montevideo.Synth
-- import Montevideo.Dispatch.Types


type ParamName = String

-- | A message for SuperCollider unaware of Vivid's type shenanigans..
type ScParams = Map ParamName Float

-- | A `ScParams'`, unlike a `Msg`, is typed for a particular kind of synth,
-- and to send it anywhere else is a type error.
-- (This innovation is Vivid's, not my own --
-- in fact I circumvent it with the `Msg` type.)
data ScParams' sdArgs where
  ScParams' :: forall params sdArgs.
          ( Vivid.VarList params
          , Vivid.Subset (Vivid.InnerVars params) sdArgs)
       => params -> ScParams' sdArgs

-- | A SuperCollider action: create a voice, destroy (free)  a voice,
-- or change a voice's parameters.
data ScAction labelType
  = ScAction_New  -- ^ create it
    { _actionSynthDefEnum :: SynthDefEnum -- ^ Kind of synth.
    , _actionSynthName    :: labelType -- ^ Instance of that kind of synth.
    , _actionScParams     :: ScParams -- ^ Can be the empty map.
      -- In fact, in mtv-lang (Montevideo.Dispatch), it always is,
      -- because voices are created in advance of being used.
    }
  | ScAction_Free -- ^ destroy it
    { _actionSynthDefEnum :: SynthDefEnum -- ^ The kind of synth.
    , _actionSynthName    :: labelType } -- ^ Which instance of the synth.
  | ScAction_Send
    { _actionSynthDefEnum :: SynthDefEnum -- ^ The kind of synth.
    , _actionSynthName    :: labelType -- ^ Which instance of the synth.
    , _actionScParams     :: ScParams } -- ^ This should not be the empty map.
  deriving (Show, Eq, Ord)
makePrisms ''ScAction

set' :: V.VividAction m => V.Synth params -> ScParams' params -> m ()
set' _synth (ScParams' m) = V.set _synth m


-- | == per-synth boilerplate

-- | = send a Map full of messages
boopScParams :: ScParams -> [ScParams' BoopParams]
boopScParams = map boopOneScParams . M.toList

moopScParams :: ScParams -> [ScParams' MoopParams]
moopScParams = map moopOneScParams . M.toList

samplerScParams :: ScParams -> [ScParams' SamplerParams]
samplerScParams = map samplerOneScParams . M.toList

sqfmScParams :: ScParams -> [ScParams' SqfmParams]
sqfmScParams = map sqfmOneScParams . M.toList

vapScParams :: ScParams -> [ScParams' VapParams]
vapScParams = map vapOneScParams . M.toList

zotScParams :: ScParams -> [ScParams' ZotParams]
zotScParams = map zotOneScParams . M.toList


-- | = send a message regarding a single parameter

boopOneScParams :: (ParamName, Float) -> ScParams' BoopParams
boopOneScParams ("on",n)    = ScParams' (toI n :: I "on")
boopOneScParams ("freq",n)  = ScParams' (toI n :: I "freq")
boopOneScParams ("amp",n)   = ScParams' (toI n :: I "amp")
boopOneScParams (param,val) = error $
  "boopOneScParams: unexpected message: " ++ show param ++ "=" ++ show val

moopOneScParams :: (ParamName, Float) -> ScParams' MoopParams
moopOneScParams ("freq",n)  = ScParams' (toI n :: I "freq")
moopOneScParams ("amp",n)   = ScParams' (toI n :: I "amp")
moopOneScParams ("lag",n)   = ScParams' (toI n :: I "lag")
moopOneScParams (param,val) = error $
  "moopOneScParams: unexpected message: " ++ show param ++ "=" ++ show val

samplerOneScParams :: (ParamName, Float) -> ScParams' SamplerParams
samplerOneScParams ("amp",n)     = ScParams' (toI n :: I "amp")
samplerOneScParams ("buffer",n)  = ScParams' (toI n :: I "buffer")
samplerOneScParams ("speed",n)   = ScParams' (toI n :: I "speed")
samplerOneScParams ("trigger",n) = ScParams' (toI n :: I "trigger")
samplerOneScParams (param,val)   = error $
  "sqfmOneScParams: unexpected message: " ++ show param ++ "=" ++ show val

sqfmOneScParams :: (ParamName, Float) -> ScParams' SqfmParams
sqfmOneScParams ("freq",n)           = ScParams' (toI n :: I "freq")
sqfmOneScParams ("amp",n)            = ScParams' (toI n :: I "amp")
sqfmOneScParams ("width",n)          = ScParams' (toI n :: I "width")
sqfmOneScParams ("width-vib-amp",n)  = ScParams' (toI n :: I "width-vib-amp")
sqfmOneScParams ("width-vib-freq",n) = ScParams' (toI n :: I "width-vib-freq")
sqfmOneScParams (param,val)          = error $
  "sqfmOneScParams: unexpected message: " ++ show param ++ "=" ++ show val

vapOneScParams :: (ParamName, Float) -> ScParams' VapParams
vapOneScParams ("freq",n)       = ScParams' (toI n :: I "freq")
vapOneScParams ("amp",n)        = ScParams' (toI n :: I "amp")
vapOneScParams ("saw",n)        = ScParams' (toI n :: I "saw")
vapOneScParams ("delay-freq",n) = ScParams' (toI n :: I "delay-freq")
vapOneScParams ("delay-amp",n)  = ScParams' (toI n :: I "delay-amp")
vapOneScParams ("fm-freq",n)    = ScParams' (toI n :: I "fm-freq")
vapOneScParams ("fm-amp",n)     = ScParams' (toI n :: I "fm-amp")
vapOneScParams ("fm2-freq",n)   = ScParams' (toI n :: I "fm2-freq")
vapOneScParams ("fm2-amp",n)    = ScParams' (toI n :: I "fm2-amp")
vapOneScParams ("nz-lpf",n)     = ScParams' (toI n :: I "nz-lpf")
vapOneScParams (param,val)      = error $
  "vapOneScParams: unexpected message: " ++ show param ++ "=" ++ show val

zotOneScParams :: (ParamName, Float) -> ScParams' ZotParams
zotOneScParams ("on",_)    = ScParams' () -- unused by Zot
zotOneScParams ("off",n)   = ScParams' (toI n :: I "off")
zotOneScParams ("freq",n)  = ScParams' (toI n :: I "freq")
zotOneScParams ("amp",n)   = ScParams' (toI n :: I "amp")
zotOneScParams ("att",n)   = ScParams' (toI n :: I "att")
zotOneScParams ("rel",n)   = ScParams' (toI n :: I "rel")
zotOneScParams ("pulse",n) = ScParams' (toI n :: I "pulse")
zotOneScParams ("fm-b",n)  = ScParams' (toI n :: I "fm-b")
zotOneScParams ("fm-m",n)  = ScParams' (toI n :: I "fm-m")
zotOneScParams ("fm-f",n)  = ScParams' (toI n :: I "fm-f")
zotOneScParams ("pm-b",n)  = ScParams' (toI n :: I "pm-b")
zotOneScParams ("pm-m",n)  = ScParams' (toI n :: I "pm-m")
zotOneScParams ("pm-f",n)  = ScParams' (toI n :: I "pm-f")
zotOneScParams ("w",n)     = ScParams' (toI n :: I "w")
zotOneScParams ("wm-b",n)  = ScParams' (toI n :: I "wm-b")
zotOneScParams ("wm-m",n)  = ScParams' (toI n :: I "wm-m")
zotOneScParams ("wm-f",n)  = ScParams' (toI n :: I "wm-f")
zotOneScParams ("am",n)    = ScParams' (toI n :: I "am")
zotOneScParams ("am-b",n)  = ScParams' (toI n :: I "am-b")
zotOneScParams ("am-f",n)  = ScParams' (toI n :: I "am-f")
zotOneScParams ("rm",n)    = ScParams' (toI n :: I "rm")
zotOneScParams ("rm-b",n)  = ScParams' (toI n :: I "rm-b")
zotOneScParams ("rm-f",n)  = ScParams' (toI n :: I "rm-f")
zotOneScParams ("lpf",n)   = ScParams' (toI n :: I "lpf")
zotOneScParams ("lpf-m",n) = ScParams' (toI n :: I "lpf-m")
zotOneScParams ("bpf",n)   = ScParams' (toI n :: I "bpf")
zotOneScParams ("bpf-m",n) = ScParams' (toI n :: I "bpf-m")
zotOneScParams ("bpf-q",n) = ScParams' (toI n :: I "bpf-q")
zotOneScParams ("hpf",n)   = ScParams' (toI n :: I "hpf")
zotOneScParams ("hpf-m",n) = ScParams' (toI n :: I "hpf-m")
zotOneScParams ("lim",n)   = ScParams' (toI n :: I "lim")
zotOneScParams ("sh",n)    = ScParams' (toI n :: I "sh")
zotOneScParams ("sh-b",n)  = ScParams' (toI n :: I "sh-b")
zotOneScParams ("del",n)   = ScParams' (toI n :: I "del")
zotOneScParams ("source-l", n) = ScParams' (toI n :: I "source-l")
zotOneScParams ("am-l"    , n) = ScParams' (toI n :: I "am-l"    )
zotOneScParams ("rm-l"    , n) = ScParams' (toI n :: I "rm-l"    )
zotOneScParams ("filt-l"  , n) = ScParams' (toI n :: I "filt-l"  )
zotOneScParams ("lim-l"   , n) = ScParams' (toI n :: I "lim-l"   )
zotOneScParams ("shift-l" , n) = ScParams' (toI n :: I "shift-l" )
zotOneScParams (param,val) = error $
  "zotOneScParams: unexpected message: " ++ show param ++ "=" ++ show val
