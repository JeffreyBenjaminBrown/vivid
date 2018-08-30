-- To use, first run `mwc <- create` once,
-- Then run either of these repeatedly:
  -- > oneSignal mwc
  -- > wholeSynth mwc
-- Maybe change the constraints (below).

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
import Vivid.Jbb.Synths


-- | Max # of params, max # of signals, max depth.
-- Should be in [1,8], [1,8], and >1, respectively.
constraints = mkRandConstraints 3 3 2


-- | Creates a value between 1 and x if log x appears in the formula below.
logRandomFreq :: RVar Double
logRandomFreq = exp . (* log 100) <$> stdUniform

randomArgs :: RVar ( I "AP1", I "AP2", I "AP3", I "AP4"
                   , I "AP5", I "AP6", I "AP7", I "AP8")
randomArgs = do
  let toHz = exp . ((-)1) . (*2) -- takes [0,1] to [1,20k]
  a <- toI . toHz <$> stdUniform
  b <- toI . toHz <$> stdUniform
  c <- toI . toHz <$> stdUniform
  d <- toI . toHz <$> stdUniform
  e <- toI . toHz <$> stdUniform
  f <- toI . toHz <$> stdUniform
  g <- toI . toHz <$> stdUniform
  h <- toI . toHz <$> stdUniform
  return $ ( a,b,c,d,e,f,g,h )

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
  s1 <- renderSig a M.empty
  out 0 [s1, s1]

-- | Generate an mwc for this using "Data.Random.create"
oneSignal mwc = do
  a <- randAbSig constraints
  print $ show a

  let sd = abSigToSD a
  s <- synth sd defaultArgs
  y <- sampleFrom mwc randomArgs
  print $ show y

  set s y
  wait 1
  free s

-- | = Try a whole synth
abSynthToSD :: AbSynth -> SynthDef TheAbParams
abSynthToSD plan = sd defaultArgs $ do
  let m = M.empty

  s1 <- renderSig ((M.!) plan AS1) m
  let m1 = M.insert AS1 s1 m

  s2 <- renderSig ((M.!) plan AS2) m1
  let m2 = M.insert AS2 s2 m1

  s3 <- renderSig ((M.!) plan AS3) m2
  let m3 = M.insert AS3 s3 m2

  out 0 [s3,s3]


-- | Generate an mwc for this using "Data.Random.create"
wholeSynth mwc = do
  a <- randAbSynth constraints
  print $ show a

  let sd = abSynthToSD a
  s <- synth sd defaultArgs

  y <- sampleFrom mwc randomArgs
  print $ show y

  set s y
  wait 1
  free s
