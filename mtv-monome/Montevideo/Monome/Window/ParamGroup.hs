{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE ScopedTypeVariables
, TypeApplications
#-}

module Montevideo.Monome.Window.ParamGroup (
    handler
  , pulseWindow
  , label
  ) where

import           Control.Lens
import qualified Data.Bimap as Bi

import           Montevideo.Monome.Types.Most
import           Montevideo.Util


-- | Two parameters are omitted: "on", which is only ever 1
-- (Dispatch needs it, I think, but Monome doesn't),
-- and "freq", which is controlled by the keyboard, not the "sliders".
data ParamGroup
  = PG_FM     -- ^ fm-m, fm-f, fm-b
  | PG_PM     -- ^ pm-m, pm-f, pm-b,
  | PG_WM     -- ^ wm-m, wm-f, wm-b, w

  | PG_source -- ^ amp, pulse
  | PG_AM     -- ^ am, am-b, am-f
  | PG_RM     -- ^ rm, rm-b, rm-f

  | PG_HLF    -- ^ hpf, hpf-m, lpf,  lpf-m
  | PG_BF     -- ^ bpf, bpf-m, bpf-q
  | PG_end    -- ^ lim, sh, sh-b, del
  deriving (Eq, Ord, Show)

groupParams :: ParamGroup -> [String]
groupParams PG_FM     = ["fm-m", "fm-f", "fm-b"] 
groupParams PG_PM     = ["pm-m", "pm-f", "pm-b,"] 
groupParams PG_WM     = ["wm-m", "wm-f", "wm-b", "w"] 
groupParams PG_source = ["amp", "pulse"] 
groupParams PG_AM     = ["am", "am-b", "am-f"] 
groupParams PG_RM     = ["rm", "rm-b", "rm-f"] 
groupParams PG_HLF    = ["hpf", "hpf-m", "lpf", "lpf-m"] 
groupParams PG_BF     = ["bpf", "bpf-m", "bpf-q"] 
groupParams PG_end    = ["lim", "sh", "sh-b, del"] 

-- | This is total in one direction; keying on a ParamGroup never fails.
groupXY :: Bi.Bimap ParamGroup (X,Y)
groupXY = Bi.fromList
  [ (PG_FM     , (0,0))
  , (PG_PM     , (1,0))
  , (PG_WM     , (2,0))
  , (PG_source , (0,1))
  , (PG_AM     , (1,1))
  , (PG_RM     , (2,1))
  , (PG_HLF    , (0,2))
  , (PG_BF     , (1,2))
  , (PG_end    , (2,2))
  ]

label :: WindowId
label = ParamGroupWindow

pulseWindow :: Window EdoApp
pulseWindow =  Window {
    windowLabel = label
  , windowContains = \(x,y) -> numBetween 0 2 x &&
                               numBetween 0 2 y
  , windowInitLeds = \_ _ -> []
  , windowHandler = handler }

handler :: St EdoApp -> (MonomeId, ((X,Y), Switch))
        -> Either String (St EdoApp)
handler    st           (mi, press@(xy,sw)) =
  -- Find what group (x,y) corresponds to.
  -- Change the (supposed) _stParamGroup value to that.
  -- Darken the old button, light the new one.
  error "TODO"
