{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module Montevideo.Monome.Presets where

import qualified Data.Bimap as B
import qualified Data.Map as M

import Montevideo.Monome.Types
import Montevideo.Synth.Zot


storePreset :: St app -> IO ()
storePreset st = let
  pairs = map showPair $ M.toList $ _stZotDefaults st where
    showPair (p,f) = "(" ++ (zotConstructors B.! p) ++ ", " ++ show f ++ ")"
  ss = [ "\n_ = M.fromList"
       , "  [ " ++ head pairs ]
       ++ map (\p -> "  , " ++ p) (tail pairs)
       ++ ["  ]"]
  in appendFile
     "mtv-monome/Montevideo/Monome/Presets.hs"
     $ unlines ss

pr1, pr2, pr3, pr4, pr5, pr6 :: M.Map ZotParam Float

-- | smooth mellow buzz
-- The AM_F causes a lot of noise if pulse is > 0.
muho = pr1 -- ^ like a muted horn
pr1 = M.fromList
  [ (Zot_amp,0.2)
  , (Zot_pulse,0.0)
  , (Zot_fm_m,0.0)
  , (Zot_fm_f,1.5258788e-5)
  , (Zot_fm_b,0.0)
  , (Zot_pm_m,0.16666667)
  , (Zot_pm_f,3.90625e-3)
  , (Zot_pm_b,0.6666667)
  , (Zot_wm_m,0.75)
  , (Zot_wm_f,0.15749013)
  , (Zot_wm_b,1.25)
  , (Zot_am,1/3)
  , (Zot_am_b,0.75)
  , (Zot_am_f,2.4803141e-2)
  ]

-- | starts with wah
pr2 = M.fromList
  [ (Zot_pulse,0.0)
  , (Zot_wm_m,-0.5)
  , (Zot_wm_f,1.5625e-2)
  , (Zot_w,0.8333333)
  , (Zot_rm,1.0)
  , (Zot_rm_b,0.75)
  , (Zot_lim,1.0)
  , (Zot_sh,1.0)
  , (Zot_sh_b,9.688727e-5)
  , (Zot_del,2.5198421)
  ]

-- | nasty if AM big, mellow warbly otherwise
loch = pr3 -- ^ reminds me of the Loch Ness Monster
pr3 = M.fromList
  [ (Zot_amp, 7.937005e-2)
  , (Zot_pulse, 8.3333336e-2)
  , (Zot_fm_m, 0.25)
  , (Zot_fm_f, 0.5)
  , (Zot_fm_b, 1.1666666)
  , (Zot_pm_m, 0.16666667)
  , (Zot_pm_f, 0.25)
  , (Zot_pm_b, 1.3333334)
  , (Zot_wm_m, 1.25)
  , (Zot_wm_f, 6.25e-2)
  , (Zot_wm_b, 2.0)
  , (Zot_w, 0.25)
  , (Zot_am, 1.0)
  , (Zot_am_b, 2.0)
  , (Zot_am_f, 6.25e-2)
  , (Zot_rm, 0.0)
  , (Zot_rm_f, 0.25)
  ]

-- | thin, fuzzy, with occasional bad pops
pr4 = M.fromList
  [ (Zot_amp, 7.937005e-2)
  , (Zot_pulse, 0.25)
  , (Zot_fm_m, 0.6666667)
  , (Zot_fm_f, 1.0)
  , (Zot_pm_m, 0.41666666)
  , (Zot_pm_f, 1.0)
  , (Zot_pm_b, 2.0)
  , (Zot_wm_m, 1.5)
  , (Zot_wm_f, 2.4803141e-2)
  , (Zot_wm_b, 1.5)
  , (Zot_w, 0.6666667)
  , (Zot_am, 0.5833333)
  , (Zot_am_b, 0.6666667)
  , (Zot_am_f, 0.25)
  , (Zot_hpf, 2.0)
  , (Zot_hpf_m, 0.0)
  , (Zot_lpf, 2.0)
  , (Zot_lpf_m, 0.0)
  , (Zot_bpf, 2.0)
  , (Zot_bpf_m, 0.0)
  , (Zot_bpf_q, 6.25e-2)
  , (Zot_lim, 6.25e-2)
  , (Zot_sh, 1.5258788e-5)
  , (Zot_sh_b, 1.5258788e-5)
  , (Zot_del, 6.25e-2)
  , (Zot_source_l, 0.0)
  , (Zot_am_l, 0.0)
  , (Zot_rm_l, 0.0)
  , (Zot_filt_l, 0.33333334)
  , (Zot_lim_l, 0.0)
  , (Zot_shift_l, 0.0)
  ]

-- | thick, fuzzy, warbly (PM)
pr5 = M.fromList
  [ (Zot_amp, 3.216662e-2)
  , (Zot_rel, 0.31622776)
  , (Zot_pulse, 0.25)
  , (Zot_pm_m, 0.33333334)
  , (Zot_pm_f, 9.843133e-3)
  , (Zot_am, 0.75)
  , (Zot_am_b, 0.9166667)
  , (Zot_am_f, 0.25)
  ]

-- | deep clean bass
pr6 = M.fromList
  [ (Zot_amp, 4.6946548e-2)
  , (Zot_att, 9.999999e-4)
  , (Zot_pulse, 0.0)
  , (Zot_fm_m, 1.0)
  , (Zot_pm_m, 0.6666667)
  , (Zot_pm_f, 6.2007853e-3)
  , (Zot_pm_b, 0.6666667)
  , (Zot_wm_m, 0.33333334)
  , (Zot_wm_f, 0.25)
  , (Zot_wm_b, -1.0)
  , (Zot_w, 0.125)
  ]

-- | like a harmonica in mid-high registers
pr7 = M.fromList
  [ (Zot_amp, 2.2039779e-2)
  , (Zot_att, 6.299605e-2)
  , (Zot_rel, 0.1709976)
  , (Zot_pulse, 0.33333334)
  , (Zot_fm_m, 1.6666666)
  , (Zot_fm_f, 1.0)
  , (Zot_fm_b, 2.0)
  ]

-- | buzzy stringlike bass/mid, fairydust highs
pr8 = M.fromList
  [ (Zot_amp, 2.2039779e-2)
  , (Zot_att, 2.817269e-3)
  , (Zot_rel, 0.1709976)
  , (Zot_pulse, 0.33333334)
  , (Zot_fm_m, 0.5)
  , (Zot_fm_f, 1.0)
  , (Zot_fm_b, 2.0)
  , (Zot_pm_m, 0.0)
  , (Zot_pm_f, 1.0)
  , (Zot_pm_b, 2.0)
  , (Zot_wm_m, 0.0)
  , (Zot_wm_f, 3.9372534e-2)
  , (Zot_am, 8.3333336e-2)
  , (Zot_am_f, 2.4803141e-2)
  , (Zot_rm, 0.0)
  , (Zot_rm_f, 9.9212565e-2)
  , (Zot_lpf, 5.656854)
  , (Zot_lpf_m, 0.0)
  ]
