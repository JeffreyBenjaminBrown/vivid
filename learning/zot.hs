-- This is the preferred feedback method; for some reason using localIn
-- and localOut sounds better than using localBuf, playBuf and recordBuf.
-- Notice that there can only be one (audio rate) localIn/localOut pair
-- per synthdef. That's no problem, because it can have lots of channels.

{-# LANGUAGE DataKinds, ExtendedDefaultRules #-}

import Vivid


type ZotParams = '["freq","amp","pulse","fm","del"]

zot :: SynthDef ZotParams
zot = sd ( 0 :: I "freq"
         , 0 :: I "amp"
         , 0 :: I "pulse"
         , 0 :: I "fm"
         , 0.01 :: I "del"
         ) $ do
  [fb_1] <- localIn(1)
  fm <- (V::V "freq") ~+ ((V::V "fm") ~* fb_1)

  aSin <- sinOsc (freq_ fm)
  aPulse <- pulse (freq_  fm)
  source <- (V::V "amp") ~* (    (      (V :: V "pulse" ) ~* aPulse)
                              ~+ ((1 ~- (V :: V "pulse")) ~* aSin) )

  s1 <- delayL( in_ source
               , maxDelaySecs_ 1
               , delaySecs_ $ (V::V "del") )

  localOut( [s1] )

  out 0 [source,source]
