-- A loop (in the CS sense, not the musical one) that can be stopped.
-- Example
  -- > on <- newMVar True
  -- > loop on           -- the sound starts
  -- > swapMVar on False -- it stops
  -- True                -- ignore this return value; it's the old value of on
  -- > swapMVar on True
  -- False
  -- > loop on           -- it starts again

{-# LANGUAGE DataKinds #-}

import Vivid
import Data.List as L
import Control.Concurrent.MVar


boop :: SynthDef '["note","amp"]
boop = sd ((0,0.1) -- default values
           :: (I "note",I "amp")) $ do
   s1 <- (V::V "amp") ~* sinOsc (freq_ (V::V "note"))
   out 0 [s1, s1]

loop :: MVar Bool -> IO ()
loop continue = do
  s <- synth boop ()
  let go = do
        set s (toI 500 :: I "note")
        c <- readMVar continue
        if c
          then do wait 0.1
                  go
          else do freeAll
                  return ()
  fork go
