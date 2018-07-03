-- Here's a loop that can be modified while running.
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
