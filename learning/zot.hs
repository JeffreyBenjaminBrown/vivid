-- next: add AM, RM, filter, shaper, delay, pitch shift.
-- then ? taps

{-# LANGUAGE DataKinds, ExtendedDefaultRules #-}

import Vivid


type ZotParams = '["amp"
  ,"pulse"                    -- pulse + sin = 1
  ,"f","fm-b","fm-m","fm-f"  -- baseline, fb mul, sin mul, sin freq
      ,"pm-b","pm-m","pm-f"            -- fb mul, sin mul, sin freq
  ,"w","wm-b","wm-m","wm-f"  -- baseline, fb mul, sin mul, sin freq
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
         , 0.01 :: I "del"
         ) $ do
  [fb_1] <- localIn(1)
  fb01 <- ((fb_1 ~+ 1) ~/ 2) -- varies in [0,1]
  fm <- (V::V "f")
        ~+ (V::V "fm-b") ~* fb_1
        ~+ (V::V"fm-m") ~* sinOsc (freq_ (V::V"fm-f"))
  pm <- (pi/2) ~* (   (V::V"pm-b") ~* fb01
                   ~+ (V::V"pm-m") ~* sinOsc (freq_ (V::V"pm-f")))
  wm <- (V :: V "w")
    ~+ 0.5 ~* (   (V::V"wm-b") ~* fb_1
               ~+ (V::V"wm-m")  ~* sinOsc (freq_ (V::V"wm-f")))

  aSin <- sinOsc  (freq_ fm, phase_ pm)
  aPulse <- pulse (freq_ fm, width_ wm)
  source <-          (V :: V "pulse" ) ~* aPulse
            ~+ (1 ~- (V :: V "pulse")) ~* aSin

  s1 <- delayL( in_ source
               , maxDelaySecs_ 1
               , delaySecs_ $ (V::V "del") )

  localOut( [s1] )

  out 0 [(V::V "amp") ~* source, (V::V "amp") ~* source]
