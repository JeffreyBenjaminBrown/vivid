module Montevideo.Monome.Presets where

import qualified Data.Bimap as B
import qualified Data.Map as M

import Montevideo.Monome.Types
import Montevideo.Synth.Zot


storePreset :: St app -> IO ()
storePreset st = let
  pairs = map showPair $ M.toList $ _stZotDefaults st where
    showPair (p,f) = "(" ++ (codeStrings B.! p) ++ ", " ++ show f ++ ")"
  ss = [ "\n_ = M.fromList"
       , "  [ " ++ head pairs ]
       ++ map (\pair -> "  , " ++ pair) pairs
       ++ ["  ]"]
  in appendFile
     "/home/jeff/mtv/mtv-monome/Montevideo/Monome/Presets.hs"
     $ unlines ss

pr1, pr2 :: M.Map ZotParam Float

-- | smooth mellow buzz
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

-- | nasty
pr3 = M.fromList
  [ (Zot_amp, 7.937005e-2)
  , (Zot_amp, 7.937005e-2)
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
