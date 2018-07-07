-- https://we.lurk.org/hyperkitty/list/livecode@we.lurk.org/thread/ZQBFCHMBFIIM36KB7S77IDAPYJKMBRF2/

{-# LANGUAGE DataKinds #-}

import Vivid

foo = sd (1 :: I "gate") $ do
  e <- adsrGen 0.2 0.1 0.6 0.7 (Curve_Curve (-4)) (gate_ (V::V "gate"))
  s <- e ~* sinOsc (freq_ $ midiCPS 50)
  out 0 [s,s]

main = do
  s <- synth foo ()
  wait 1
  free s
