-- TODO
  -- next ? taps
  -- should ? frequencies all be expressed as multiples of the fundamental

-- Notes on usage
  -- The AM and RM scale factors of 2 would cause no change in the integral
  -- of the signal if the modulator was a triangle wave. A modulator that
  -- spent more time at the extremes would cause its volume to rise.

  -- `am-b` and `rm-b`, like `pulse`, add to the feedback input while
  -- subtracting from a competing one. `fm-b`, `pm-b`, `wm-b` do not.

{-# LANGUAGE DataKinds, ExtendedDefaultRules #-}

module Vivid.Synths.Zot where

import Vivid
import Vivid.LongVarLists


type ZotParams = '["amp"
  ,"pulse"                    -- pulse + sin = 1
  ,"f", "fm-b","fm-m","fm-f"  -- baseline, fb mul, sin mul, sin freq
      , "pm-b","pm-m","pm-f"            -- fb mul, sin mul, sin freq
  ,"w", "wm-b","wm-m","wm-f"  -- baseline, fb mul, sin mul, sin freq
  ,"am","am-b",       "am-f" -- amSig = am * am'd carrier + (1-am) * carrier
                             -- am'd carrier = am-b * fb + (1 - am-b) * sin
  ,"rm","rm-b",       "rm-f" -- same as am, just no bias
  ,"lpf","lpf-m"
  ,"bpf","bpf-m","bpf-q"
  ,"hpf","hpf-m"
  ,"lim"                     -- lim = x  =>  |signal| will not exceed x
  ,"sh", "sh-b"              -- pitch shift: baseline, feedback mul
  ,"del"]

zot :: SynthDef ZotParams
zot = sd ( 0 :: I "amp"
         , 0.5 :: I "pulse"
         , 0 :: I "f"
         , 0 :: I "fm-b"
         , 0 :: I "fm-m"
         , 0 :: I "fm-f"
         , 0 :: I "pm-b"
         , 0 :: I "pm-m"
         , 0 :: I "pm-f"
         , 0.5 :: I "w"
         , 0 :: I "wm-b"
         , 0 :: I "wm-m"
         , 0 :: I "wm-f"
         , 0 :: I "am"
         , 0 :: I "am-b"
         , 0 :: I "am-f"
         , 0 :: I "rm"
         , 0 :: I "rm-b"
         , 0 :: I "rm-f"
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
  fb_1 <- head <$> localIn(1)
  fb01 <- (fb_1 ~+ 1) ~/ 2
  fm <- (V::V "f")
        ~+ (V::V "fm-b") ~* fb_1
        ~+ (V::V"fm-m") ~* sinOsc (freq_ $ (V::V"f") ~* (V::V"fm-f"))
  pm <- (pi/2) ~* (   (V::V"pm-b") ~* fb01
                   ~+ (V::V"pm-m")
                      ~* sinOsc (freq_ $ (V::V"f") ~* (V::V"pm-f")))
  wm <- (V :: V "w")
    ~+ 0.5 ~* (   (V::V"wm-b") ~* fb_1
               ~+ (V::V"wm-m")
                  ~* sinOsc (freq_ $ (V::V"f") ~* (V::V"wm-f")))

  aSin <- sinOsc  (freq_ fm, phase_ pm)
  aPulse <- pulse (freq_ fm, width_ wm)
  source <-          (V :: V "pulse" ) ~* aPulse
            ~+ (1 ~- (V :: V "pulse")) ~* aSin

  amSin <- (sinOsc (freq_ $ (V::V"f") ~* (V::V"am-f")) ~+ 1) ~/ 2
  am <- 2 ~* -- scale by 2 because AM makes it quieter
        (    fb01  ~*       (V::V"am-b")
          ~+ amSin ~* (1 ~- (V::V"am-b")) )
  amSig <- (1 ~- (V::V"am")) ~* source
           ~+    (V::V"am")  ~* source ~* am

  rmSin <- sinOsc (freq_ $ (V::V"f") ~* (V::V"rm-f"))
  rm <- 2 ~* -- scale by 2 because RM makes it quieter
        (    fb_1  ~*       (V::V"rm-b")
          ~+ rmSin ~* (1 ~- (V::V"rm-b")) )
  -- The `rmSig` formula could be simplified, I think, by
  -- pulling the constant term into the `rm` signal
  rmSig <- (1 ~- (V::V"rm")) ~* amSig
           ~+    (V::V"rm")  ~* amSig ~* rm

  filt_1 <- (1 ~- (V::V"lpf-m")) ~*          rmSig
            ~+    (V::V"lpf-m")  ~* lpf( in_ rmSig
                                       , freq_ $ (V::V"f") ~* (V::V"lpf"))
  filt_2 <- (1 ~- (V::V"bpf-m")) ~*          filt_1
            ~+    (V::V"bpf-m")  ~* bpf( in_ filt_1
                                       , freq_ $ (V::V"f") ~* (V::V"bpf")
                                       , rq_ (V::V"bpf-q"))
  filt   <- (1 ~- (V::V"hpf-m")) ~*          filt_2
            ~+    (V::V"hpf-m")  ~* hpf( in_ filt_2
                                       , freq_ $ (V::V"f") ~* (V::V"hpf"))

  lim <- tanh' (filt ~/ (V::V"lim")) ~* (V::V"lim")

  shift <- freqShift ( in_ lim
                     , freq_ $ (V::V"f") ~* (    (V::V"sh")
                                              ~+ (V::V"sh-b") ~* fb_1) )

  s1 <- delayL( in_ shift
               , maxDelaySecs_ 1
               , delaySecs_ $ (V::V "del") ~/ (V::V"f") )

  localOut( [s1] )

  out 0 [(V::V "amp") ~* shift, (V::V "amp") ~* shift]
