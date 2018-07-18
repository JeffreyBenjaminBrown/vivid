-- | Why won't `boopMessage` compile?
-- I need a function that returns messages of different types
-- but all acceptable to a particular kind of `Synth`.

{-# LANGUAGE DataKinds
           , ExtendedDefaultRules
           , ScopedTypeVariables
           , GADTs #-}

import Vivid

boop :: SynthDef '["freq","amp"]
boop = sd ( (0,0.01) -- default values
            :: (I "freq",I "amp")) $ do
  s1 <- (V::V "amp") ~* sinOsc (freq_ (V::V "freq"))
  out 0 [s1, s1]

boopMessage 0 = (toI 444 :: I "freq")
boopMessage 1 = (toI 0.1 :: I "amp")
boopMessage _ = ()
