module Vivid.Synths.Samples (
    FileSubPath, Description, Filename
  , Sample(..)
  , samplePaths        -- ^ M.Map Nickname FilePath
  , sampleDescriptions -- ^ M.Map Nickname Description
  ) where

import Control.Lens
import Data.Tuple (swap)
import qualified Data.Map as M


type FileSubPath = String -- ^ `FilePath` is already defined in Base
type Description = String
type Filename = String -- ^ without a path

data Sample = SampleKd
            | SampleKm
            | SampleKp
            | SampleKr
            | SampleKt
            | SampleSm_m
            | SampleSm_peb
            | SampleSm_pes
            | SampleSp_t
            | SampleSp_r
            | SampleSp_rf
            | SampleSl_j
            | SampleSl_b
            | SampleSl_blip
            | SampleSl_g
            | SampleSl_puff
            | SampleSl_cc
            | SampleSl_ps
            | SampleSh_cl
            | SampleSh_ch
            | SampleSh_ph
            | SampleSh_rc
            | SampleSh_he
            | SampleHl_phr
            | SampleHl_ph
            | SampleHl_dc
            | SampleHl_cgi
            | SampleHl_cg
            | SampleHl_tfc
            | SampleHl_et
            | SampleHl_scr
            | SampleHs_lc
            | SampleHs_hc
            | SampleHs_sh
            | SampleHs_shr
            | SampleHs_so
            | SampleHs_et
  deriving (Eq, Ord, Show)

rootFolder :: FilePath
rootFolder = "/home/jeff/code/Tidal/Dirt-Samples"

samplePaths :: M.Map Sample FilePath
samplePaths = M.fromList $ map f samples' where
  f ((_,nick),path) = (nick,path)

sampleDescriptions :: M.Map Sample Description
sampleDescriptions = M.fromList $ map (swap . fst) samples

samples' :: [((Description, Sample), FilePath)]
samples' = map (_2 %~ f) samples where
  f (subfolder,filename) =
    rootFolder ++ "/" ++ subfolder ++ "/" ++ filename ++ ".wav"

samples :: [((Description, Sample), (FileSubPath, Filename))]
samples =
  [ ( ("kick, damp", SampleKd)
    , ("kicklinn", "Linn Kick 1"))
  , ( ("kick, mellow", SampleKm)
    , ("bd", "BT0A0A7"))
  , ( ("kick, punch", SampleKp)
    , ("clubkick", "1"))
  , ( ("kick, rubber", SampleKr)
    , ("hardkick", "VEC1 BD Distortion 52"))
  , ( ("kick, thunder", SampleKt)
    , ("popkick", "3"))

  , ( ("snare, moderate: mellow", SampleSm_m)
    , ("drumtraks","009_DT Snare"))
  , ( ("snare, moderate: punch, electric, brief", SampleSm_peb)
    , ("dr55","003_DR55 snare"))
  , ( ("snare, moderate: pop, echo, sand", SampleSm_pes)
    , ("hh27","012_hh27snare2" ))

  , ( ("snare, punch: tight", SampleSp_t)
    , ("short","sampleoftheday-gtt-snare-drum-012"))
  , ( ("snare, punch: ring", SampleSp_r)
    , ("gretsch","snarehard"))
  , ( ("snare, punch: rip, fleeting", SampleSp_rf)
    , ("ul","ulsnare"))

  , ( ("snare, light: jungle", SampleSl_j)
    , ("jungle","jungle4snare1"))
  , ( ("snare, light: very light, brush", SampleSl_b)
    , ("gretsch","brushsnareghost"))
  , ( ("snare, light: blip", SampleSl_blip)
    , ("tech","tn1snare2"))
  , ( ("snare, light: glitch", SampleSl_g)
    , ("v","004_v_snare01"))
  , ( ("snare, light: puff", SampleSl_puff)
    , ("db","dbs12snare2"))
  , ( ("snare, light: light crack, comb-filtered", SampleSl_cc)
    , ("ab","011_ab2snare2"))
  , ( ("snare, light: punch, sand", SampleSl_ps)
    , ("db","dbs12snare1"))

  , ( ("snare, hard: close low", SampleSh_cl)
    , ("sequential","005_Tom Snare"))
  , ( ("snare, hard: hard close high", SampleSh_ch)
    , ("odx","001_DX_Snare_1"))
  , ( ("snare, hard: pop-hiss", SampleSh_ph)
    , ("incoming","000_Mattel  Snare"))
  , ( ("snare, hard: rip, close", SampleSh_rc)
    , ("ul","ulnoisey-snare"))
  , ( ("snare, hard: heavy, echo", SampleSh_he)
    , ("less","snare"))

  , ( ("hat, light: pop hiss ring", SampleHl_phr)
    , ("hh","007_hh3openhh"))
  , ( ("hat, light: pop hiss", SampleHl_ph)
    , ("sid","007_hihat01"))
  , ( ("hat, light: damp closed", SampleHl_dc)
    , ("hh","000_hh3closedhh"))
  , ( ("hat, light: closing, insistent", SampleHl_cgi)
    , ("gretsch","closedhathard"))
  , ( ("hat, light: closing", SampleHl_cg)
    , ("gretsch","closedhat"))
  , ( ("hat, light: thump foot closed", SampleHl_tfc)
    , ("gretsch","foothat"))
  , ( ("hat, light: electric tsk", SampleHl_et)
    , ("voodoo","001_VoodooHihat"))
  , ( ("hat, light: scratch can", SampleHl_scr)
    , ("feel","hihat029a"))

  , ( ("hat, sharp: light closed", SampleHs_lc)
    , ("linnhats","1"))
  , ( ("hat, sharp: hard closed", SampleHs_hc)
    , ("hc","HHCD2"))
  , ( ("hat, sharp: sharp half", SampleHs_sh)
    , ("linnhats","2"))
  , ( ("hat, sharp: sharp half ring", SampleHs_shr)
    , ("linnhats","3"))
  , ( ("hat, sharp: sharp open", SampleHs_so)
    , ("linnhats","4"))
  , ( ("hat, sharp: electric tsk", SampleHs_et)
    , ("linnhats","5"))
  ]
