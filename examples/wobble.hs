-- copied without modification from http://www.vivid-synth.com/, 2018 07 01

{-# LANGUAGE DataKinds #-}

import Vivid

wobble = sd (0 :: I "note") $ do
   s <- 50 ~* sinOsc (freq_ 10) ? KR
   s1 <- 0.1 ~* sinOsc (freq_ $ midiCPS (V::V "note") ~+ s)
   out 0 [s1, s1]

main = do
   s <- synth wobble ()
   let notes = take 12 $
         [ x | x <- [38..], (x `mod` 12) `elem` [0,3,5] ]
   forM_ (cycle notes) $ \note -> do
      set s (toI note :: I "note")
      wait 0.2
