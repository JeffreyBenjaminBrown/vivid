{-# LANGUAGE DataKinds, ExtendedDefaultRules #-}

import Vivid

foo :: SynthDef '[]
foo = sd () $ do
   l <- localBuf (numChans_ 1, numFrames_ 100)
   fb <- playBuf (buf_ l, loop_ 1)
--   pluck <- percGen( attackSecs_ 0.005 -- TODO
--                   , releaseSecs_ 0.005
--                   , level_ 4
----                   , curve_ (Curve_Curve $ 0)
--                   , doneAction_ 0 )
   s <- lpf( in_ $ (0.1 ~* whiteNoise) ~+ (0.999 ~* fb)
           , freq_ 1500 )
   recordBuf (buf_ l, in_ s, loop_ 1)
   out 0 [s,s]

main = do
  s <- synth foo ()
  wait 4
  free s
