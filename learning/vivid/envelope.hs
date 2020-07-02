-- Based on an example by Tom Murphy:
-- https://we.lurk.org/hyperkitty/list/livecode@we.lurk.org/thread/ZQBFCHMBFIIM36KB7S77IDAPYJKMBRF2/

-- The signal grows to unity for 1s, decays for 1s, then plateaus at 0.3.
-- After main has counted to 5 it sends a gate=0 signal,
-- which triggers a second of release.

{-# LANGUAGE DataKinds, ExtendedDefaultRules #-}

import Vivid

foo = sd (1 :: I "gate") $ do
  e <- adsrGen 1 1 0.3 1
    (Curve_Curve $ 0)
    (gate_ (V::V "gate"))
  s <- e ~* sinOsc (freq_ 500)
  out 0 [s,s]

count 0 = (putStrLn $ show 0) >> return ()
count n = do putStrLn $ show n
             wait 1
             count $ n-1

main = do
  s <- synth foo ()
  count 5
  set s (0 :: I "gate")
  count 3
  free s
