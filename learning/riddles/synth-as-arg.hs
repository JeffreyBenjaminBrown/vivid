-- Unfinished. I want to be able to hand synths as arguments to the
-- `loop` function. This compiles, but (in open code) the line
-- `s <- synth boop ()` generates an error -- so I can't make the synth
-- I would like to pass as an argument to `loop`.

{-# LANGUAGE DataKinds #-}

import Vivid
import Data.List as L
import Control.Concurrent.MVar

boop :: SynthDef '["note","amp"]
boop = sd ((0,0.1) -- default values
           :: (I "note",I "amp")) $ do
   s1 <- (V::V "amp") ~* sinOsc (freq_ (V::V "note"))
   out 0 [s1, s1]

loop :: (Elem "note" sdArgs)
  => MVar Bool -> Synth sdArgs -> IO ()
loop continue aSynth = do
  c <- readMVar continue
  if c
    then do set aSynth (toI 500 :: I "note")
            wait 0.1
            loop continue aSynth
    else return ()
