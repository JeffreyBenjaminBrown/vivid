-- I wrote this so I could ask GHCI for the type signature of `f`
-- (which I then included in the code).
-- This is based on Tom Murphy's code suggested here:
  -- https://we.lurk.org/hyperkitty/list/livecode@we.lurk.org/thread/25W4GB76BWXONUSUEMW5NV6Y6OO6NPAR/

{-# LANGUAGE DataKinds #-}

import Vivid

boop :: SynthDef '["freq","amp"]
boop = sd ((0,0.1) -- default values
           :: (I "freq",I "amp")) $ do
   s1 <- (V::V "amp") ~* sinOsc (freq_ (V::V "freq"))
   out 0 [s1, s1]

f :: IO (Synth '["freq", "amp"] -> Integer -> IO ())
f = pick ( [ \s n -> set s (toI n :: I "freq")
           , \s n -> set s (toI n :: I "amp")
           ] )

main = do
  s <- synth boop ()
  set' <- f
  -- run it four times so that both amp and freq are likely to be set
  set' s 400
  set' s 400
  set' s 400
  set' s 400
