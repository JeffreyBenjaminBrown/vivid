-- Parameter interpretation:
  -- Parameters ending in "-b" control how much of the feedback signal
  -- is mixed in somewhere.
  -- `am-b` and `rm-b`, like `pulse`, add in the feedback signal while
  -- subtracting from a competing one.
  -- Every other `*-b` parameter adds in feedback without
  -- any compensating reduction elsewhere.

-- PITFALL: Adding more parameters can generate a very mysterious warning.
-- That's because the Instances for VarList only reach up to tuples of
-- 38 parameters. If you ask for something with 39, it'll give you a
-- "can't match type _ with type _" error, rather than a "no VarList instance"
-- error like you might expect. The solution is to edit
-- Montevideo.Vivid.LongVarLists.

{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE DataKinds, ExtendedDefaultRules #-}

module Montevideo.Synth.Zot where

import qualified Data.Map as M
import qualified Data.Bimap as B
import Vivid hiding (Log)

import Montevideo.Vivid.LongVarLists ()
import Montevideo.Synth.Config
import Montevideo.Util


-- | See the definition of `ZotParams` (plural) for what each means.
data ZotParam
  = Zot_off
  | Zot_freq
  | Zot_amp
  | Zot_att
  | Zot_rel
  | Zot_pulse

  | Zot_fm_m
  | Zot_fm_f
  | Zot_fm_b
  | Zot_pm_m
  | Zot_pm_f
  | Zot_pm_b
  | Zot_wm_m
  | Zot_wm_f
  | Zot_wm_b
  | Zot_w

  | Zot_am
  | Zot_am_b
  | Zot_am_f
  | Zot_rm
  | Zot_rm_b
  | Zot_rm_f

  | Zot_hpf
  | Zot_hpf_m
  | Zot_lpf
  | Zot_lpf_m
  | Zot_bpf
  | Zot_bpf_m
  | Zot_bpf_q

  | Zot_lim
  | Zot_sh
  | Zot_sh_b
  | Zot_del

  | Zot_source_l
  | Zot_am_l
  | Zot_rm_l
  | Zot_filt_l
  | Zot_lim_l
  | Zot_shift_l

  deriving (Eq, Ord)

instance Show ZotParam where
  show = (strings B.!)

strings :: B.Bimap ZotParam String
strings = B.fromList
  [ (Zot_off   , "off")
  , (Zot_freq  , "freq")

  , (Zot_amp   , "amp")
  , (Zot_att   , "att")
  , (Zot_rel   , "rel")
  , (Zot_pulse , "pulse")

  , (Zot_fm_m  , "fm-m")
  , (Zot_fm_f  , "fm-f")
  , (Zot_fm_b  , "fm-b")
  , (Zot_pm_m  , "pm-m")
  , (Zot_pm_f  , "pm-f")
  , (Zot_pm_b  , "pm-b")
  , (Zot_wm_m  , "wm-m")
  , (Zot_wm_f  , "wm-f")
  , (Zot_wm_b  , "wm-b")
  , (Zot_w     , "w")

  , (Zot_am    , "am")
  , (Zot_am_b  , "am-b")
  , (Zot_am_f  , "am-f")
  , (Zot_rm    , "rm")
  , (Zot_rm_b  , "rm-b")
  , (Zot_rm_f  , "rm-f")

  , (Zot_hpf   , "hpf")
  , (Zot_hpf_m , "hpf-m")
  , (Zot_lpf   , "lpf")
  , (Zot_lpf_m , "lpf-m")
  , (Zot_bpf   , "bpf")
  , (Zot_bpf_m , "bpf-m")
  , (Zot_bpf_q , "bpf-q")

  , (Zot_lim   , "lim")
  , (Zot_sh    , "sh")
  , (Zot_sh_b  , "sh-b")
  , (Zot_del   , "del")

  , (Zot_source_l , "source-l")
  , (Zot_am_l     , "am-l")
  , (Zot_rm_l     , "rm-l")
  , (Zot_filt_l   , "filt-l")
  , (Zot_lim_l    , "lim-l")
  , (Zot_shift_l  , "shift-l")
  ]

zotConstructors :: B.Bimap ZotParam String
zotConstructors = B.fromList
  [ (Zot_off   , "Zot_off")
  , (Zot_freq  , "Zot_freq")
  , (Zot_amp   , "Zot_amp")
  , (Zot_att   , "Zot_att")
  , (Zot_rel   , "Zot_rel")
  , (Zot_pulse , "Zot_pulse")

  , (Zot_fm_m  , "Zot_fm_m")
  , (Zot_fm_f  , "Zot_fm_f")
  , (Zot_fm_b  , "Zot_fm_b")
  , (Zot_pm_m  , "Zot_pm_m")
  , (Zot_pm_f  , "Zot_pm_f")
  , (Zot_pm_b  , "Zot_pm_b")
  , (Zot_wm_m  , "Zot_wm_m")
  , (Zot_wm_f  , "Zot_wm_f")
  , (Zot_wm_b  , "Zot_wm_b")
  , (Zot_w     , "Zot_w")

  , (Zot_am    , "Zot_am")
  , (Zot_am_b  , "Zot_am_b")
  , (Zot_am_f  , "Zot_am_f")
  , (Zot_rm    , "Zot_rm")
  , (Zot_rm_b  , "Zot_rm_b")
  , (Zot_rm_f  , "Zot_rm_f")

  , (Zot_hpf   , "Zot_hpf")
  , (Zot_hpf_m , "Zot_hpf_m")
  , (Zot_lpf   , "Zot_lpf")
  , (Zot_lpf_m , "Zot_lpf_m")
  , (Zot_bpf   , "Zot_bpf")
  , (Zot_bpf_m , "Zot_bpf_m")
  , (Zot_bpf_q , "Zot_bpf_q")

  , (Zot_lim   , "Zot_lim")
  , (Zot_sh    , "Zot_sh")
  , (Zot_sh_b  , "Zot_sh_b")
  , (Zot_del   , "Zot_del")

  , (Zot_source_l, "Zot_source_l")
  , (Zot_am_l, "Zot_am_l")
  , (Zot_rm_l, "Zot_rm_l")
  , (Zot_filt_l, "Zot_filt_l")
  , (Zot_lim_l, "Zot_lim_l")
  , (Zot_shift_l, "Zot_shift_l")
  ]

zotDefaultValues :: M.Map String Float
zotDefaultValues =
  M.mapKeys (strings B.!)
  $ M.map fst _zotDefaultInfo

zotDefaultRanges :: M.Map ZotParam (NumScale, Rational, Rational)
zotDefaultRanges = M.map snd _zotDefaultInfo

-- | The range can be changed from the default during play.
_zotDefaultInfo :: M.Map ZotParam ( Float
                                 , (NumScale, Rational, Rational))
_zotDefaultInfo = M.fromList
  [ (Zot_off   , (0, (Lin, 0, 1)))     -- Monome ignores.
  , (Zot_freq  , (0, (Log, 40, 20e3))) -- Monome determines via Keyboard
                                       -- rather than ParamVal window.
  , (Zot_amp   , (defaultAmp, let top = 0.2
                              in (Log, top * 2^^(-8), top) ) )
  , (Zot_att   , (0.003, (Log, 0.001, 0.5)))
  , (Zot_rel   , (0.05, (Log, 0.05, 2)))
  , (Zot_pulse , (0.5, (Lin, 0, 1)))
  , (Zot_fm_m  , (0, (Lin, 0, 2)))
  , (Zot_fm_f  , (1, (Log, 2^^(-16), 1)))
  , (Zot_fm_b  , (0, (Lin, 0, 2)))
  , (Zot_pm_m  , (0, (Lin, 0, 4)))
  , (Zot_pm_f  , (1, (Log, 2^^(-8), 1)))
  , (Zot_pm_b  , (0, (Lin, 0, 2)))
  , (Zot_wm_m  , (0, (Lin, 0,0.5)))
  , (Zot_wm_f  , (1, (Log, 2^^(-8), 1)))
  , (Zot_wm_b  , (0, (Lin, -1,2)))
  , (Zot_w     , (0.5, (Lin, 0, 1/2))) -- width is symmetrical over [0,1]
  , (Zot_am    , (0, (Lin, 0, 1)))
  , (Zot_am_f  , (1, (Log, 2^^(-8), 1)))
  , (Zot_am_b  , (0, (Lin, 0, 1)))
  , (Zot_rm    , (0, (Lin, 0, 1)))
  , (Zot_rm_f  , (1, (Log, 2^^(-8), 1)))
  , (Zot_rm_b  , (0, (Lin, 0, 1)))
  , (Zot_lpf   , (filterFreqMax, (Log, 0.5, 2^^5)))
  , (Zot_lpf_m , (0, (Lin, 0, 1)))
  , (Zot_bpf   , (7, (Log, 1, 32)))
  , (Zot_bpf_m , (0, (Lin, 0, 1)))
  , (Zot_bpf_q , (0.5, (Log, 2^^(-4), 2^^3)))
  , (Zot_hpf   , (1, (Log, 2, 2^^8)))
  , (Zot_hpf_m , (0, (Lin, 0, 1)))
  , (Zot_lim   , (1, (Log, 2^^(-4), 2^^4)))
  , (Zot_sh    , (0, (Log, 2^^(-16), 1)))
  , (Zot_sh_b  , (0, (Log, 2^^(-16), 1)))
  , (Zot_del   , (1, (Log, 2^^(-4), 2^^4)))

  , (Zot_source_l , (0, (Lin, 0, 1)))
  , (Zot_am_l     , (0, (Lin, 0, 1)))
  , (Zot_rm_l     , (0, (Lin, 0, 1)))
  , (Zot_filt_l   , (0, (Lin, 0, 1)))
  , (Zot_lim_l    , (0, (Lin, 0, 1)))
  , (Zot_shift_l  , (1, (Lin, 0, 1)))
  ]

-- | For details on the meaning of these parameters,
-- see the comments above their appearances in the definition of `zot`.
type ZotParams = '[
  -- The originating waveform.
  -- FM applies to both, PM to the sinewave only, WM (width mod)
  -- to the pulse wave only. Width=0.5 => square wave.
   "off"
  ,"freq"                   -- baseline freq
  ,"amp"                    -- amplitude
  ,"att"                    -- attack time in seconds
  ,"rel"                    -- release time in seconds
  ,"pulse"                  -- pulse + sin = 1
  ,"fm-b","fm-m","fm-f"     -- fb mul, sin mul, sin freq
  ,"pm-b","pm-m","pm-f"     -- fb mul, sin mul, sin freq
  ,"wm-b","wm-m","wm-f","w" -- fb mul, sin mul, sin freq, baseline

  -- Serially next: Each of these
  ,"am","am-b","am-f" -- amSig = am * am'd carrier + (1-am) * carrier
                      -- am'd carrier = am-b * fb + (1 - am-b) * sin
  ,"rm","rm-b","rm-f" -- RM is the same as AM, except no bias.
  ,"lpf","lpf-m"
  ,"bpf","bpf-m","bpf-q" -- Small bpf-q <=> narrow band.
  ,"hpf","hpf-m"
  ,"lim"        -- lim = x  =>  |signal| will not exceed x
  ,"sh", "sh-b" -- pitch shift: baseline, feedback mul
  ,"del"        -- delay in periods, maximum 1 second

  , "source-l"  -- level; how much to tap that part of the signal chain
  , "am-l"      -- level; how much to tap that part of the signal chain
  , "rm-l"      -- level; how much to tap that part of the signal chain
  , "filt-l"    -- level; how much to tap that part of the signal chain
  , "lim-l"     -- level; how much to tap that part of the signal chain
  , "shift-l"   -- level; how much to tap that part of the signal chain
  ]

zot :: SynthDef ZotParams
zot = sd ( toI $ zotDefaultValues M.! "off" :: I "off"
         , toI $ zotDefaultValues M.! "freq" :: I "freq"
         , toI $ zotDefaultValues M.! "amp" :: I "amp"
         , toI $ zotDefaultValues M.! "att" :: I "att"
         , toI $ zotDefaultValues M.! "rel" :: I "rel"
         , toI $ zotDefaultValues M.! "pulse" :: I "pulse"
         , toI $ zotDefaultValues M.! "fm-b" :: I "fm-b"
         , toI $ zotDefaultValues M.! "fm-m" :: I "fm-m"
         , toI $ zotDefaultValues M.! "fm-f" :: I "fm-f"
         , toI $ zotDefaultValues M.! "pm-b" :: I "pm-b"
         , toI $ zotDefaultValues M.! "pm-m" :: I "pm-m"
         , toI $ zotDefaultValues M.! "pm-f" :: I "pm-f"
         , toI $ zotDefaultValues M.! "wm-b" :: I "wm-b"
         , toI $ zotDefaultValues M.! "wm-m" :: I "wm-m"
         , toI $ zotDefaultValues M.! "wm-f" :: I "wm-f"
         , toI $ zotDefaultValues M.! "w" :: I "w"
         , toI $ zotDefaultValues M.! "am" :: I "am"
         , toI $ zotDefaultValues M.! "am-b" :: I "am-b"
         , toI $ zotDefaultValues M.! "am-f" :: I "am-f"
         , toI $ zotDefaultValues M.! "rm" :: I "rm"
         , toI $ zotDefaultValues M.! "rm-b" :: I "rm-b"
         , toI $ zotDefaultValues M.! "rm-f" :: I "rm-f"
         , toI $ zotDefaultValues M.! "lpf" :: I "lpf"
         , toI $ zotDefaultValues M.! "lpf-m" :: I "lpf-m"
         , toI $ zotDefaultValues M.! "bpf" :: I "bpf"
         , toI $ zotDefaultValues M.! "bpf-m" :: I "bpf-m"
         , toI $ zotDefaultValues M.! "bpf-q" :: I "bpf-q"
         , toI $ zotDefaultValues M.! "hpf" :: I "hpf"
         , toI $ zotDefaultValues M.! "hpf-m" :: I "hpf-m"
         , toI $ zotDefaultValues M.! "lim" :: I "lim"
         , toI $ zotDefaultValues M.! "sh" :: I "sh"
         , toI $ zotDefaultValues M.! "sh-b" :: I "sh-b"
         , toI $ zotDefaultValues M.! "del" :: I "del"
         , toI $ zotDefaultValues M.! "source-l" :: I "source-l"
         , toI $ zotDefaultValues M.! "am-l" :: I "am-l"
         , toI $ zotDefaultValues M.! "rm-l" :: I "rm-l"
         , toI $ zotDefaultValues M.! "filt-l" :: I "filt-l"
         , toI $ zotDefaultValues M.! "lim-l" :: I "lim-l"
         , toI $ zotDefaultValues M.! "shift-l" :: I "shift-l"
         ) $ do

  -- Feedback from the `localOut` at the end of this definition.
  -- So named because it has no built-in bias; if we're lucky it ranges
  -- from -1 to 1. (Feedback can do weird things, though.)
  fb_11 <- head <$> localIn(1)

  -- `fb_01 fb_11 signal, rescaled to the range [0,1],
  -- assuming fb_11 ranges in [-1,1].
  fb_01 <- (fb_11 ~+ 1) ~/ 2

  -- `fm` modulates the frequency of the `aSin` signal below.
  -- (It used to modulate `aPulse` also, but that sounds staticky and
  -- very inharmonic for almost all audio-rate fm.)
  -- `fm-f` is how fast their frequency wobbles, in terms of the fundamental.
  -- `fm-m` controls the "amplitude" in Hz of the wobble.
  -- `fm-b` controls how much magical feedback is part of `fm`.
  -- It uses `fb_11`, not `fb_01`, because `fm` wobbles on both sides
  -- of the target frequency. (If the feedback is biased, which can happen,
  -- then the `fm` will be, too.)
  fm <-    (V::V "freq")
        ~+ (V::V "fm-b") ~* fb_11
        ~+ ( (V::V"fm-m") ~*
             (V::V"freq") ~*
             sinOsc (freq_ $ (V::V"freq") ~* (V::V"fm-f") ) )

  -- Pulse modulation for aSin. PITFALL: When pulse=1,
  -- this has no effect, because we hear only aPulse, not aSin.
  -- `pm-f` controls the frequency of the wobble, scaled by the fundamental.
  -- `pm-a` controls its amplitude.
  -- `pm-b` is magical feedback.
  pm <- (pi/2) ~* (   (V::V"pm-b") ~* fb_01
                   ~+ (V::V"pm-m")
                      ~* sinOsc (freq_ $ (V::V"freq") ~*
                                         (V::V"pm-f") ) )

  -- Width modulation for aPulse. PITFALL: When pulse=0,
  -- this has no effect, because we hear only aSin, not aPulse.
  -- `w` is the baseline value, to which the modulation is added.
  -- `wm-f` controls the frequency of the wobble.
  -- `pm-a` controls its amplitude.
  -- `pm-b` is magical feedback.
  wm <- (V :: V "w")
    ~+ 0.5 ~* (   (V::V"wm-b") ~* fb_11
               ~+ (V::V"wm-m")
                  ~* sinOsc (freq_ $ (V::V"freq") ~* (V::V"wm-f")))

  -- `aSin` and `aPulse` are the two fundamental signals.
  aSin   <- sinOsc (freq_ fm             , phase_ pm)
  aPulse <- pulse  (freq_ (V :: V "freq"), width_ wm)
  source <- (          (V :: V "pulse" ) ~* aPulse
              ~+ (1 ~- (V :: V "pulse")) ~* aSin )
            ~* ( envGen_wGate
                 1 -- The gate "signal" starts at 1 and never changes.
                 (V::V "att")
                 (env 0 [(1,1)] Curve_Lin)
                 DoNothing )
            ~* ( envGen_wGate
                 (V::V "off")
                 (V::V "rel")
                 (env 1 [(0,1)] Curve_Lin)
                 DoNothing )

  -- This next section runs `source` through AM,
  -- and then runs the output of that through FM
  --
  -- The difference between `am` and `rm` is just that
  -- `am` uses a sinewave with a minimum of 0.
  -- Unlike other Ms, AM and RM are unusual in that there is no "width"
  -- to choose; the sinewave scaling the source does not vary in amplitude.
  --
  -- Let X in the following denote AM or RM.
  -- The `x-f` parameter determines the frequency of the modulation.
  -- The `x` parameter determines how much X-of-source to mix with source.
  --   If `x=1`, we hear nothing but the X-modulated signal.
  -- The `x-b` parameter determines how much feedback to use for modulation.
  --   If `x-b`=0, the modulation comes entirely from a sinewave.

  amSin <- ( sinOsc ( freq_ $ (V::V"freq") ~* (V::V"am-f") )
             ~+ 1 ) ~/ 2
  am <- 2 ~* -- scale by 2 because AM reduces volume.
             -- (The integral of (sin x + 1)/2 from 0 to 2pi is roughly pi.)
        (    fb_01 ~*       (V::V"am-b")
          ~+ amSin ~* (1 ~- (V::V"am-b")) )
  amSig <- (1 ~- (V::V"am")) ~* source
           ~+    (V::V"am")  ~* source ~* am

  rmSin <- sinOsc (freq_ $ (V::V"freq") ~* (V::V"rm-f"))
  rm <- (2 * pi / 4) ~* -- Scale because RM makes it quieter.
                        -- (The integral of abs (sin x) from 0 to 2pi is 4.)
        (    fb_11 ~*       (V::V"rm-b")
          ~+ rmSin ~* (1 ~- (V::V"rm-b")) )
  rmSig <- (1 ~- (V::V"rm")) ~* amSig
           ~+    (V::V"rm")  ~* amSig ~* rm

  -- These filters act serially on the RM output.
  -- The default parameters are such that they have no effect.
  -- Only the bandpass filter has a "q" parameter.
  -- TODO ? These are linear filters. SC offers a lot of other ones;
  -- I imagine some of those offer "q" parameters.

  let freqRange :: SDBody' ZotParams Signal -> SDBody' ZotParams Signal
      freqRange = biOp Min filterFreqMax .
                  biOp Max filterFreqMin
  filt_1 <- (1 ~- (V::V"lpf-m")) ~* rmSig
            ~+ (  (V::V"lpf-m")  ~*
                  lpf ( in_ rmSig
                      , freq_ $ freqRange $
                        (V::V"freq") ~* (V::V"lpf") ) )
  filt_2 <- (1 ~- (V::V"bpf-m")) ~* filt_1
            ~+ (  (V::V"bpf-m")  ~*
                  bpf ( in_ filt_1
                      , freq_ $ freqRange $
                        (V::V"freq") ~* (V::V"bpf")
                      , rq_ (V::V"bpf-q") ) )
  filt_3 <- (1 ~- (V::V"hpf-m")) ~* filt_2
            ~+ (  (V::V"hpf-m")  ~*
                  hpf ( in_ filt_2
                      , freq_ $ freqRange $
                        (V::V"freq") ~* (V::V"hpf") ) )

  -- This clamps filt_3. High values of `lim` mean *less* clamping.
  lim <- tanh' (filt_3 ~/ (V::V"lim")) ~* (V::V"lim")

  -- Shift the frequency of `lim`.
  -- (All frequencies shift equally in Hz, destroying harmonicity.)
  -- `sh` is an additive pitch shift. Negative values are reasonable.
  -- `sh-b` adds feedback into the amount shifted by.
  -- It seems natural to omit any "mix" parameter between those, so I did.
  shift <- freqShift ( in_ lim
                     , freq_ $   (V::V"freq") ~*
                               ( (V::V"sh") ~+
                                 (V::V"sh-b") ~* fb_11) )

  -- `del` dictates how many periods (the inverse of frequency)
  -- to delay the feedback by.
  -- Max delay in seconds is set to 1, which seems like hardly a constraint --
  -- for instance, for a 100 Hz wave, I think I'm unlikely to want to
  -- delay it by more than 100 waveforms. (I imagine typical del values
  -- will range in (0,10) or so, usually erring toward 0.)
  -- PITFALL: Zero and negative values don't make sense here,
  -- because the feedback needs some delay.
  s1 <- delayL( in_ shift
              , maxDelaySecs_ 1
              , delaySecs_ $ (V::V "del") ~/ (V::V"freq") )

  -- The source of the feedback.
  localOut( [s1] )

  let x = (V::V "amp")
          ~* ( (V::V "source-l") ~* source ~+
               (V::V "am-l")     ~* amSig  ~+
               (V::V "rm-l")     ~* rmSig  ~+
               (V::V "filt-l")   ~* filt_3 ~+
               (V::V "lim-l")    ~* lim    ~+
               (V::V "shift-l")  ~* shift )
          ~/ ( biOp Max 0.001 -- This avoids a divide-by-zero.
               ( (V::V "source-l") ~+ -- This normalizes the volume, so that
                 (V::V "am-l")     ~+ -- tapping multiple sources is no
                 (V::V "rm-l")     ~+ -- louder than tapping just one.
                 (V::V "filt-l")   ~+
                 (V::V "lim-l")    ~+
                 (V::V "shift-l") ) )
    in out 0 [ x, x ]
