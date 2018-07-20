{-# LANGUAGE ViewPatterns #-}

--module Vivid.Jbb.RandomSynth where
--
--import Vivid


-- | Every random synth has up to eight parameters.
data RParam = RP1 | RP2 | RP3 | RP4 | RP5 | RP6 | RP7 | RP8
  deriving (Show, Eq, Ord)

rParam :: Int -> Int -> RParam
rParam maxParam n = if n <= min maxParam 8 && n >= 1
  then [RP1, RP2, RP3, RP4, RP5, RP6, RP7, RP8] !! (n-1)
  else error $ show n ++ " is not the number of an RParam."

-- | An `RSigName` identifies a previously named `RSig`. For instance,
-- in `s1 <- sinOsc (freq_ (V::V "freq"))`, we have created a signal
-- named `s1`. 
data RSigName = RS1 | RS2 | RS3 | RS4 | RS5 | RS6 | RS7 | RS8
  deriving (Show, Eq, Ord)

rSigName :: Int -> Int -> RSigName
rSigName maxSigName n = if n <= min maxSigName 8 && n >= 1
  then [RS1, RS2, RS3, RS4, RS5, RS6, RS7, RS8] !! (n-1)
  else error $ show n ++ " is not the number of an RSigName."

data RSig = RSig RSigName
          | RV RParam
          | RSigFormula RFormula
          | RSigGen RGen

data RFormula = RProd RSig RSig
              | RSum RSig RSig

data RGen = RSin RSinMsg
          | RSaw RSawMsg

data RSinMsg = RSinMsg { rSinFreq :: RSig, rSinPhase :: RSig }
data RSawMsg = RSawMsg { rSawFreq :: RSig }
