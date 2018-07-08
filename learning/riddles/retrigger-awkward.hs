-- Based on an example by Tom Murphy:
-- https://we.lurk.org/hyperkitty/list/livecode@we.lurk.org/thread/ZQBFCHMBFIIM36KB7S77IDAPYJKMBRF2/

-- Retriggering an ADSR envelope is awkward.
-- (But maybe other envelopes are different?)
-- See comments in `main` for details.

{-# LANGUAGE DataKinds, ExtendedDefaultRules, ViewPatterns #-}

import Vivid

foo = sd (1 :: I "gate") $ do
  e <- adsrGen 1 1 0.5 1
    (Curve_Curve $ 0)
    (gate_ (V::V "gate"))
  s <- e ~* sinOsc (freq_ (500 ~+ (100 ~* e)))
  out 0 [s,s]

mySong = doScheduledIn 0.1 $ do
   fork $ do
      wait 1
      s1 <- synth foo ()
      wait 1
      free s1
   s2 <- synth foo ()
   wait 1.5
   free s2

test pauseLength = doScheduledIn 0.1 $ test' pauseLength

test' pauseLength = do
  s <- synth foo () -- starts the ADSR envelope, with a default gate=1 vlaue
  wait 2

  -- I can retrigger the AD phase, IF the R phase is allowed to progress
  -- for at least a milisecond. If the pause between the gate=0
  -- and gate=1 messages is much less than that,
  -- the AD phase is not retriggered; the synth stays in the R phase.
  -- (To see this, try running `test 0.001` and then `test 0.0001`.)
  set s (0 :: I "gate")
  wait pauseLength
  set s (1 :: I "gate")
  wait 2

  set s (0 :: I "gate") -- trigger the R portion of the curve
  wait 2

  free s
