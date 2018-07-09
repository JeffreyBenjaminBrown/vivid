-- Awkward: You've got to "close" the ADSR envelope, and then wait for
-- about a milisecond, before you can "open" it again, raising it above
-- its sustaining level. See comments in `main` for details.

-- Based on an example by Tom Murphy:
  -- https://we.lurk.org/hyperkitty/list/livecode@we.lurk.org/thread/ZQBFCHMBFIIM36KB7S77IDAPYJKMBRF2/

{-# LANGUAGE DataKinds, ExtendedDefaultRules, ViewPatterns #-}

import Vivid

foo :: SynthDef '["gate"]
foo = sd (1 :: I "gate") $ do
  e <- adsrGen 1 1 0.5 1
    (Curve_Curve $ 0)
    (gate_ (V::V "gate"))
  s <- e ~* sinOsc (freq_ (500 ~+ (100 ~* e)))
  out 0 [s,s]

test pauseLength = doScheduledIn 0.1 $ test' pauseLength

test' pauseLength = do
  s <- synth foo () -- starts the ADSR envelope, with a default gate=1 vlaue
  wait 2

  -- I can retrigger the AD phase, IF the R phase is allowed to progress
  -- for at least a milisecond. If the pause between the gate=0
  -- and gate=1 messages is substsantially less than that,
  -- the AD phase is not retriggered; the synth stays in the S phase.
  -- (To see this, try running `test 0.001` vs. `test 0.0001`.)
  set s (0 :: I "gate")
  wait pauseLength
  set s (1 :: I "gate")
  wait 2

  set s (0 :: I "gate") -- trigger the R portion of the curve
  wait 2

  free s
