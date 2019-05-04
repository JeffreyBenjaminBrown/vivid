module Vivid.Synths.Samples (
    samplePaths        -- ^ M.Map Nickname FilePath
  , sampleDescriptions -- ^ M.Map Nickname Description
  ) where

import Control.Lens
import qualified Data.Map as M

import Vivid.Dispatch.Types hiding (samples)


rootFolder :: FilePath
rootFolder = "/home/jeff/code/Tidal/Dirt-Samples"

samplePaths :: M.Map Nickname FilePath
samplePaths = M.fromList $ map f samples' where
  f ((_,nick),path) = (nick,path)

sampleDescriptions :: M.Map Nickname Description
sampleDescriptions = M.fromList $ map fst samples

samples' :: [((Description, Nickname), FilePath)]
samples' = map (_2 %~ f) samples where
  f (subfolder,filename) =
    rootFolder ++ "/" ++ subfolder ++ "/" ++ filename ++ ".wav"

samples :: [((Description, Nickname), (FileSubPath, Filename))]
samples =
  [ ( ("kick, damp", "kd")
    , ("kicklinn", "Linn Kick 1"))
  , ( ("kick, mellow", "km")
    , ("bd", "BT0A0A7"))
  , ( ("kick, punch", "kp")
    , ("clubkick", "1"))
  , ( ("kick, rubber", "kr")
    , ("hardkick", "VEC1 BD Distortion 52"))
  , ( ("kick, thunder", "kt")
    , ("popkick", "3"))

  , ( ("snare, moderate: mellow", "sm_m")
    , ("drumtraks","009_DT Snare"))
  , ( ("snare, moderate: punch, electric, brief", "sm_peb")
    , ("dr55","003_DR55 snare"))
  , ( ("snare, moderate: pop, echo, sand", "sm_pes")
    , ("hh27","012_hh27snare2" ))

  , ( ("snare, punch: tight", "sp_t")
    , ("short","sampleoftheday-gtt-snare-drum-012"))
  , ( ("snare, punch: ring", "sp_r")
    , ("gretsch","snarehard"))
  , ( ("snare, punch: rip, fleeting", "sp_rf")
    , ("ul","ulsnare"))

  , ( ("snare, light: jungle", "sl_j")
    , ("jungle","jungle4snare1"))
  , ( ("snare, light: very light, brush", "sl_b")
    , ("gretsch","brushsnareghost"))
  , ( ("snare, light: blip", "sl_blip")
    , ("tech","tn1snare2"))
  , ( ("snare, light: glitch", "sl_g")
    , ("v","004_v_snare01"))
  , ( ("snare, light: puff", "sl_puff")
    , ("db","dbs12snare2"))
  , ( ("snare, light: light crack, comb-filtered", "sl_cc")
    , ("ab","011_ab2snare2"))
  , ( ("snare, light: punch, sand", "sl_ps")
    , ("db","dbs12snare1"))

  , ( ("snare, hard: close low", "sh_cl")
    , ("sequential","005_Tom Snare"))
  , ( ("snare, hard: hard close high", "sh_ch")
    , ("odx","001_DX_Snare_1"))
  , ( ("snare, hard: pop-hiss", "sh_ph")
    , ("incoming","000_Mattel  Snare"))
  , ( ("snare, hard: rip, close", "sh_rc")
    , ("ul","ulnoisey-snare"))
  , ( ("snare, hard: heavy, echo", "sh_he")
    , ("less","snare"))

  , ( ("hat, light: pop hiss ring", "hl_phr")
    , ("hh","007_hh3openhh"))
  , ( ("hat, light: pop hiss", "hl_ph")
    , ("sid","007_hihat01"))
  , ( ("hat, light: damp closed", "hl_dc")
    , ("hh","000_hh3closedhh"))
  , ( ("hat, light: closing, insistent", "hl_cgi")
    , ("gretsch","closedhathard"))
  , ( ("hat, light: closing", "hl_cg")
    , ("gretsch","closedhat"))
  , ( ("hat, light: thump foot closed", "hl_tfc")
    , ("gretsch","foothat"))
  , ( ("hat, light: electric tsk", "hl_et")
    , ("voodoo","001_VoodooHihat"))
  , ( ("hat, light: scratch can", "hl_scr")
    , ("feel","hihat029a"))

  , ( ("hat, sharp: light closed", "hs_lc")
    , ("linnhats","1"))
  , ( ("hat, sharp: hard closed", "hs_hc")
    , ("hc","HHCD2"))
  , ( ("hat, sharp: sharp half", "hs_sh")
    , ("linnhats","2"))
  , ( ("hat, sharp: sharp half ring", "hs_shr")
    , ("linnhats","3"))
  , ( ("hat, sharp: sharp open", "hs_so")
    , ("linnhats","4"))
  , ( ("hat, sharp: electric tsk", "hs_et")
    , ("linnhats","5"))
  ]
