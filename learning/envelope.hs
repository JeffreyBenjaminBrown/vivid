-- example by Tom Murphy:
-- https://we.lurk.org/hyperkitty/list/livecode@we.lurk.org/thread/ZQBFCHMBFIIM36KB7S77IDAPYJKMBRF2/

{-# LANGUAGE DataKinds #-}

import Vivid

foo = sd (1 :: I "gate") $ do
  e <- adsrGen
    (0.2::Double)  -- Float or Double works
    (0.1::Double)  -- Float or Double works
    (0.6::Double)  -- Float or Double works
    (0.7::Double)  -- Float or Double works
    (Curve_Curve $ -4)
    (gate_ (V::V "gate"))
  s <- e ~* sinOsc (freq_ (500 :: Double)) -- Float or Double works
  out (0::Integer) [s,s] -- Int or Integer works

main = do
  s <- synth foo ()
  wait 1
  free s
