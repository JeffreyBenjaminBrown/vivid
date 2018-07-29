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
  s <- synth foo () -- start the ADSR envelope, with a default gate=1 vlaue
  wait 2

  set s (0 :: I "gate") -- trigger the R phase
  -- The next "gate" signal will only retrigger the AD phase
  -- if the R phase is allowed to progress for at least a milisecond.
  -- (To see this, try running `test 0.001` vs. `test 0.0001`.)
  wait pauseLength

  set s (1 :: I "gate") -- retrigger the AD phase
  wait 2

  free s
