{-# LANGUAGE ViewPatterns #-}

--module Vivid.Jbb.RandomSynth where
--
import Vivid


-- | Without RPossible we would usually create invalid signal graphs
-- TODO ? How to ensure that previously created signals are actually used?
data RPossible = RPossible { maxParams :: Int
                           , nNamedSignals :: Int }


-- | = RSig: The top of the signals category
data RSig = RSigFormula RFormula
          | RSigGen RGen
          | RSig RSigName -- ^ a previously constructed RSig
          | RV RParam
  deriving (Show, Eq, Ord)

rSig :: RPossible -> IO RSig -- TODO
rSig (RPossible maxParamms nNamedSigs) = return $ RV RP1


-- | = RFormula
data RFormula = RProd RSig RSig
              | RSum RSig RSig
  deriving (Show, Eq, Ord)

rFormula :: RPossible -> IO RFormula
rFormula poss = do f <- pick [RProd, RSum]
                   a <- rSig poss
                   b <- rSig poss
                   return $ f a b


-- | = RGen
data RGen = RSin RSinMsg
          | RSaw RSawMsg
  deriving (Show, Eq, Ord)

rGen :: RPossible -> IO RGen
rGen poss = do x <- pick [rSaw poss, rSin poss]
               x -- this doesn't return `x`, it "does" `x`


-- | = RSin
rSin :: RPossible -> IO RGen
rSin poss = RSin <$> rSinMsg poss

data RSinMsg = RSinMsg { rSinFreq :: RSig, rSinPhase :: RSig }
  deriving (Show, Eq, Ord)

rSinMsg :: RPossible -> IO RSinMsg -- TODO
rSinMsg _ = return $ RSinMsg (RV RP1) (RV RP2)


-- | = RSaw
rSaw :: RPossible -> IO RGen
rSaw poss = RSaw <$> rSawMsg poss

data RSawMsg = RSawMsg { rSawFreq :: RSig }
  deriving (Show, Eq, Ord)

rSawMsg :: RPossible -> IO RSawMsg -- TODO
rSawMsg _ = return $ RSawMsg (RV RP1)


-- | An `RSigName` identifies a previously named `RSig`. For instance,
-- in `s1 <- sinOsc (freq_ (V::V "freq"))`, we have created a signal
-- named `s1`. 
data RSigName = RS1 | RS2 | RS3 | RS4 | RS5 | RS6 | RS7 | RS8
  deriving (Show, Eq, Ord)

theRSigNames = [RS1, RS2, RS3, RS4, RS5, RS6, RS7, RS8]

sigName :: Int -> Int -> RSigName
sigName maxSigName n = if n <= min maxSigName 8 && n >= 1
  then [RS1, RS2, RS3, RS4, RS5, RS6, RS7, RS8] !! (n-1)
  else error $ show n ++ " is not the number of an RSigName."

rSigName :: RPossible -> IO RSigName
rSigName (RPossible _ maxSigName) = pick $ take maxSigName theRSigNames


-- | = Every random synth has up to eight parameters.
data RParam = RP1 | RP2 | RP3 | RP4 | RP5 | RP6 | RP7 | RP8
  deriving (Show, Eq, Ord)

theRParams = [RP1, RP2, RP3, RP4, RP5, RP6, RP7, RP8]

param :: Int -> Int -> RParam
param maxParam n = if n <= min maxParam 8 && n >= 1
  then [RP1, RP2, RP3, RP4, RP5, RP6, RP7, RP8] !! (n-1)
  else error $ show n ++ " is not the number of an RParam."

rParam :: RPossible -> IO RParam
rParam (RPossible maxParam _) = pick $ take maxParam theRParams
