{-# LANGUAGE DataKinds, ExtendedDefaultRules #-}

import Vivid


main :: IO (Synth '["gate"])
main = do
   s <- synth foo ()
   putStrLn "Now we fade in!"
   set s (1 ::I "gate")
   return s

e :: EnvLiterally '["gate"]
e = env 0                  -- Start at 0.
  [ (1, 0.1),(1,1),(0,0.1) -- Fade in fast, hold, fade out.
  , (1,0.1),(0,0.1)]       -- Fade in and out again fast.
  Curve_Lin

foo :: SynthDef '["gate"]
foo = sd (1 ::I "gate") $ do
   e' <- envGen_wGate (V::V "gate") 1 e DoNothing
   s <- sinOsc (freq_ 440) ~* e'
   out 0 [s,s]
