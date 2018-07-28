-- This is the preferred feedback method; for some reason using localIn
-- and localOut sounds better than using localBuf, playBuf and recordBuf.
-- Notice that there can only be one (audio rate) localIn/localOut pair
-- per synthdef. That's no problem, because it can have lots of channels.

{-# LANGUAGE DataKinds, ExtendedDefaultRules #-}

import Vivid

foo :: SynthDef '[]
foo = sd () $ do
   [s_fb,t_fb] <- localIn(2)

   s <- lpf( in_ $  (0.1 ~* whiteNoise)
                 ~+ (0.999 ~* toSig s_fb)
           , freq_ 1500 )
   s' <- delayL( in_ s
                   , maxDelaySecs_ 1
                   , delaySecs_ 0.01 )

   t <- lpf( in_ $  (0.1 ~* whiteNoise)
                 ~+ (0.999 ~* toSig t_fb)
           , freq_ 1500 )
   t' <- delayL( in_ t
                   , maxDelaySecs_ 1
                   , delaySecs_ 0.0075 )

   localOut( [s',t'] )
   out 0 [s,t]

main = do
  s <- synth foo ()
  wait 4
  free s
