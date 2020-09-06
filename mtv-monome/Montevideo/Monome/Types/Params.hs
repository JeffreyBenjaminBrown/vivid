{-# LANGUAGE TemplateHaskell #-}

module Montevideo.Monome.Types.Params where

import           Control.Lens
import           Data.Either.Combinators
import qualified Data.Bimap as Bi

import           Montevideo.Monome.Types.Monome
import           Montevideo.Synth


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
  | PG_levels -- ^ "source-l", "am-l", "rm-l", "filt-l", "lim-l", "shift-l"
  deriving (Eq, Ord, Show)
makePrisms ''ParamGroup

paramGroup_params :: ParamGroup -> [ZotParam]
paramGroup_params PG_FM     = [Zot_fm_m, Zot_fm_f, Zot_fm_b]
paramGroup_params PG_PM     = [Zot_pm_m, Zot_pm_f, Zot_pm_b]
paramGroup_params PG_WM     = [Zot_wm_m, Zot_wm_f, Zot_wm_b, Zot_w]

paramGroup_params PG_source = [Zot_amp, Zot_pulse]
paramGroup_params PG_AM     = [Zot_am, Zot_am_f, Zot_am_b]
paramGroup_params PG_RM     = [Zot_rm, Zot_rm_f, Zot_rm_b]

paramGroup_params PG_HLF    = [Zot_hpf, Zot_hpf_m, Zot_lpf, Zot_lpf_m]
paramGroup_params PG_BF     = [Zot_bpf, Zot_bpf_m, Zot_bpf_q]
paramGroup_params PG_end    = [Zot_lim, Zot_sh, Zot_sh_b, Zot_del]

paramGroup_params PG_levels = [ Zot_source_l, Zot_am_l, Zot_rm_l
                              , Zot_filt_l, Zot_lim_l, Zot_shift_l ]

paramGroup_toParam :: ParamGroup -> Int -> Either String ZotParam
paramGroup_toParam pg i =
  mapLeft ("paramGroup_toParam: " ++) $
  case drop i $ paramGroup_params pg of
    [] -> Left ( "ParamGroup " ++ show pg ++
                 " has fewer than " ++ show i ++ " parameters." )
    (s:_) -> Right s

-- | This is total in one direction; keying on a ParamGroup never fails.
-- (In that case it's clearer to use `paramGroup_toXy`, below.)
paramGroupXys :: Bi.Bimap ParamGroup (X,Y)
paramGroupXys = Bi.fromList
  [ (PG_FM     , (0,0))
  , (PG_PM     , (1,0))
  , (PG_WM     , (2,0))
  , (PG_source , (0,1))
  , (PG_levels , (0,3))
  , (PG_AM     , (1,1))
  , (PG_RM     , (2,1))
  , (PG_HLF    , (0,2))
  , (PG_BF     , (1,2))
  , (PG_end    , (2,2))
  ]

paramGroup_toXy :: ParamGroup -> (X,Y)
paramGroup_toXy = (Bi.!) paramGroupXys
