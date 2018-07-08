{-# LANGUAGE DataKinds, ExtendedDefaultRules #-}

import Vivid

boop :: SynthDef '["f1","f2"]
boop = sd (400::I"f1", 300::I"f2") $ do
   s1 <- 0.1 ~* sinOsc (freq_ (V::V"f1"))
   s2 <- s1 ~- (0.1 ~* sinOsc (freq_ (V::V"f2")) )
   s1 <- s1 ~* s2 -- This line does not do what is intended. Instead
     -- it creates a new signal called `s1` that masks the old one.
     -- (That's a property of do-notation, not a problem with Vivid.)
   out 0 [s1, s1]

main :: IO ()
main = do
   s <- synth boop ()
   wait 1
   free s
