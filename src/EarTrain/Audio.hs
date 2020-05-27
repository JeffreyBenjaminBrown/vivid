{-# LANGUAGE ScopedTypeVariables
, DataKinds
#-}

module EarTrain.Audio where

import Vivid
import Synths


-- | = Making sounds

playFreqs :: (Real a, Floating a) => [a] -> IO ()
playFreqs freqs = do
  let msg a = (toI a :: I "freq", 0.1 :: I "amp")
  synths <- mapM (synth boopPulse . msg) freqs
  wait (2 :: Int)
  mapM_ free synths

et12toFreq :: Floating a => a -> a -> a
et12toFreq baseFreq p = 2**(p/31) * baseFreq
