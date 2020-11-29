{-# LANGUAGE DataKinds, ExtendedDefaultRules #-}

module Montevideo.Synth.Distortion where

import Vivid


busNum = 23
voiceLevel = 0.05 -- Levels near 1 seem to distort.
dist = 30
volume = 0.3 / voiceLevel

effect :: SynthDef '["in", "out"]
effect = sd (23 ::I "in", 0 ::I "out") $ do
   i <- aIn (bus_ (V::V "in"))
   -- s <- clip2 i 1
   s <- tanh' (dist ~* i) ~* volume ~/ dist
   out' [s,s]

origin :: SynthDef '["out", "freq"]
origin = sd ( 0   :: I "out"
            , 400 :: I "freq") $ do
   s <- voiceLevel ~* sinOsc (freq_ (V::V "freq"))
   out' [s,s]

main :: IO ()
main = do
   fx <- synth effect (toI busNum :: I "in")

   -- When we're not feeding one synth into another one, we usually don't care
   -- what order synths get processed. When we do, input comes before output:
   in1 <- synthBefore fx origin ( toI busNum :: I "out"
                                , 1000 / 3 :: I "freq" )
   wait 1
   in2 <- synthBefore fx origin ( toI busNum :: I "out"
                                , 1250 / 3 :: I "freq" )
   in3 <- synthBefore fx origin ( toI busNum :: I "out"
                                , 1500 / 3 :: I "freq" )
   wait 2
   free fx
   -- mapM_ free [in1,in2,in3]
