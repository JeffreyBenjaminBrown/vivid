{-# LANGUAGE DataKinds
, ExtendedDefaultRules
, FlexibleContexts -- just for the random-fu hack
#-}

import qualified Data.Map as M

import Data.Random
import System.Random.MWC

import Vivid
import Vivid.Jbb.Random.Types
import Vivid.Jbb.Random.RandomSignal
import Vivid.Jbb.Random.RandomSynth
import Vivid.Jbb.Random.Render


cs = mkRandConstraints 2 2 2

logRandomFreq :: RVar Double
logRandomFreq = exp . (* log 20000) <$> stdUniform

logNormal :: Double -> Double -> RVar Double
logNormal mu sigmaSq = do
  x <- normal mu sigmaSq
  return (exp x)

-- | Create an mwc with the "create" function.
-- I don't know how to seed random-fu, so I hold onto the source of entropy.
aRandom mwc = do
  y <- sampleFrom mwc (logNormal 5 1)
  print y
  return mwc

t = exp . (* log 20000) <$> stdNormal

defaultArgs = ( 0 :: I "AP1"
              , 0 :: I "AP2"
              , 0 :: I "AP3"             
              , 0 :: I "AP4"
              , 0 :: I "AP5"
              , 0 :: I "AP6"             
              , 0 :: I "AP7"
              , 0 :: I "AP8" )

abSigToSD :: AbSig -> SynthDef TheAbParams
abSigToSD a = sd defaultArgs $ do
  s1 <- renderSig M.empty a
  out 0 [s1, s1]

-- main = do
--   a <- randAbSig cs
--   sd <- abSigToSD a
