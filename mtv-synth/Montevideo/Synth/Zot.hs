-- Parameter interpretation:
  -- Parameters ending in "-b" control how much of the feedback signal
  -- is mixed in somewhere.
  -- `am-b` and `rm-b`, like `pulse`, add in the feedback signal while
  -- subtracting from a competing one.
  -- Every other `*-b` parameter adds in feedback without
  -- any compensating reduction elsewhere.

{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# LANGUAGE DataKinds, ExtendedDefaultRules #-}

module Montevideo.Synth.Zot where

import Vivid

import Montevideo.Vivid.LongVarLists ()
import Montevideo.Synth.Config


-- | For details on the meaning of these parameters,
-- see the comments above their appearances in the definition of `zot`.
type ZotParams = '[
  -- The originating waveform.
  -- FM applies to both, PM to the sinewave only, WM (width mod)
  -- to the pulse wave only. Width=0.5 => square wave.
  "on", "amp"
  ,"pulse"                   -- pulse + sin = 1
  ,"freq"                    -- baseline freq
  ,"fm-b","fm-m","fm-f"      -- fb mul, sin mul, sin freq
  , "pm-b","pm-m","pm-f"     -- fb mul, sin mul, sin freq
  , "wm-b","wm-m","wm-f","w" -- fb mul, sin mul, sin freq, baseline

  -- Serially next: Each of these
  ,"am","am-b","am-f" -- amSig = am * am'd carrier + (1-am) * carrier
                      -- am'd carrier = am-b * fb + (1 - am-b) * sin
  ,"rm","rm-b","rm-f" -- RM is the same as AM, except no bias.
  ,"lpf","lpf-m"
  ,"bpf","bpf-m","bpf-q"
  ,"hpf","hpf-m"
  ,"lim"                     -- lim = x  =>  |signal| will not exceed x
  ,"sh", "sh-b"              -- pitch shift: baseline, feedback mul
  ,"del"]                    -- delay in periods, maximum 1 second

zot :: SynthDef ZotParams
zot = sd ( 0 :: I "on"
         , toI defaultAmp :: I "amp"
         , 0.5 :: I "pulse"
         , 0 :: I "freq"
         , 0 :: I "fm-b"
         , 0 :: I "fm-m"
         , 1 :: I "fm-f"
         , 0 :: I "pm-b"
         , 0 :: I "pm-m"
         , 1 :: I "pm-f"
         , 0 :: I "wm-b"
         , 0 :: I "wm-m"
         , 1 :: I "wm-f"
         , 0.5 :: I "w"
         , 0 :: I "am"
         , 0 :: I "am-b"
         , 1 :: I "am-f"
         , 0 :: I "rm"
         , 0 :: I "rm-b"
         , 1 :: I "rm-f"
         , 22050 :: I "lpf" -- any higher than this and it freaks out
         , 0 :: I "lpf-m"
         , 300 :: I "bpf"
         , 0 :: I "bpf-m"
         , 0.5 :: I "bpf-q"
         , 1 :: I "hpf" -- negative and it freaks out
         , 0 :: I "hpf-m"
         , 1 :: I "lim"
         , 0 :: I "sh"
         , 0 :: I "sh-b"
         , 1 :: I "del"
         ) $ do

  -- Feedback from the `localOut` at the end of this definition.
  -- So named because it has no built-in bias; if we're lucky it ranges
  -- from -1 to 1. (Feedback can do weird things, though.)
  fb_11 <- head <$> localIn(1)

  -- `fb_01 fb_11 signal, rescaled to the range [0,1],
  -- assuming fb_11 ranges in [-1,1].
  fb_01 <- (fb_11 ~+ 1) ~/ 2

  -- `fm` modulates the frequency of the `aSin` and `aPulse` signals below.
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
  --   TODO ? Does width outside of [0,1] make sense?
  --   The docs say that's its range, and width=0.5 <=> square wave.
  --   http://doc.sccode.org/Classes/Pulse.html
  wm <- (V :: V "w")
    ~+ 0.5 ~* (   (V::V"wm-b") ~* fb_11
               ~+ (V::V"wm-m")
                  ~* sinOsc (freq_ $ (V::V"freq") ~* (V::V"wm-f")))

  -- `aSin` and `aPulse` are the two fundamental signals.
  aSin   <- sinOsc (freq_ fm, phase_ pm)
  aPulse <- pulse  (freq_ fm, width_ wm)
  source <-          (V :: V "pulse" ) ~* aPulse
            ~+ (1 ~- (V :: V "pulse")) ~* aSin

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

  filt_1 <- (1 ~- (V::V"lpf-m")) ~*           rmSig
            ~+    (V::V"lpf-m")  ~* lpf ( in_ rmSig
                                        , freq_ $ (V::V"freq") ~*
                                                  (V::V"lpf") )
  filt_2 <- (1 ~- (V::V"bpf-m")) ~*           filt_1
            ~+    (V::V"bpf-m")  ~* bpf ( in_ filt_1
                                        , freq_ $ (V::V"freq") ~*
                                                  (V::V"bpf")
                                        , rq_ (V::V"bpf-q"))
  filt_3   <- (1 ~- (V::V"hpf-m")) ~*           filt_2
              ~+    (V::V"hpf-m")  ~* hpf ( in_ filt_2
                                          , freq_ $ (V::V"freq") ~*
                                                    (V::V"hpf"))

  -- This clamps filt_3. High values of `lim` mean *less* clamping.
  lim <- tanh' (filt_3 ~/ (V::V"lim")) ~* (V::V"lim")

  -- Shift the frequency of `lim`. (This shifts all frequencies equally
  -- in Hz, so it destroys harmonicity.)
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

  out 0 [ (V::V "on") ~* (V::V "amp") ~* shift
        , (V::V "on") ~* (V::V "amp") ~* shift ]
