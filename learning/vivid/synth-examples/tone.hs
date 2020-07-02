-- copied without modification from http://www.vivid-synth.com/, 2018 07 01

{-# LANGUAGE DataKinds #-}

import Vivid

tone = sd (0::I "note") $ do
   a <- lfTri (freq_ 0.2) ? KR ~* 0.5 ~+ 0.5
   freq <- lag (in_ $ midiCPS (V::V "note"), lagSecs_ 1.25) ? KR
   b <- 0.1 ~* varSaw (freq_ freq, width_ a)
   out 0 [b, b]

main = do
   s <- synth tone (45::I "note")
   forever $ forM_ [45, 57, 64, 55] $ \freq -> do
      set s (freq :: I "note")
      wait 2.5
