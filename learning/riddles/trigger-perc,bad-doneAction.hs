{-# LANGUAGE DataKinds, ExtendedDefaultRules #-}

import Vivid

foo :: SynthDef '["attackSecs","releaseSecs","level","doneAction"]
foo = sd (1 :: I "attackSecs"
         ,1 :: I "releaseSecs"
         ,1 :: I "level"
         ,0 :: I "doneAction") $ do
  e <- percGen (attackSecs_ (V::V"attackSecs")
               ,releaseSecs_ (V::V"releaseSecs")
               ,level_ (V::V"level")
               ,doneAction_ (V::V"doneAction"))
  s <- e ~* sinOsc (freq_ (500 ~+ (100 ~* e)))
  out 0 [s,s]

test = doScheduledIn 0.1 test' where
  test' = do
    s <- synth foo ()
    wait 2
    set s (1 :: I "level")
    wait 4
    free s
