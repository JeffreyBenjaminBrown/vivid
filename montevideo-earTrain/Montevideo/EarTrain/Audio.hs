{-# LANGUAGE ScopedTypeVariables
, DataKinds
#-}

module Montevideo.EarTrain.Audio where

import Vivid
import Synths
import EarTrain.Types


-- | = Making sounds

playFreqs :: (Real a, Floating a) => [a] -> IO ()
playFreqs freqs = do
  let msg a = (toI a :: I "freq", 0.1 :: I "amp")
  synths <- mapM (synth boopPulse . msg) freqs
  wait (2 :: Int)
  mapM_ free synths

edoValToFreq :: Floating a => Edo -> a -> a -> a
edoValToFreq edo baseFreq p =
  2**(p/fromIntegral edo) * baseFreq
