-- Here's a loop (in the CS sense, not the musical one)
-- that can be modified as it runs.
-- Example:
  -- > x <- newMVar (444 :: Int)
  -- > f x
  -- > swapMVar x 555 -- the old value 444 returned here doesn't matter

{-# LANGUAGE DataKinds #-}

import Vivid
import Data.List as L
import Control.Concurrent.MVar

boop :: SynthDef '["note","amp"]
boop = sd ((0,0.1) -- default values
           :: (I "note",I "amp")) $ do
   s1 <- (V::V "amp") ~* sinOsc (freq_ (V::V "note"))
   out 0 [s1, s1]

abc :: VividAction m => m [Node '["note", "amp"]]
abc = do
  a <- synth boop ()
  b <- synth boop ()
  c <- synth boop ()
  return [a,b,c]

f :: MVar Int -> IO ()
f m = do
  s <- synth boop ()
  fork $ loop s
  where
    loop s = do
      freq <- readMVar m
      set s (toI freq :: I "note")
      wait 0.1
      loop s
