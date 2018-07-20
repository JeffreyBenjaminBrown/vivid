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

rSig :: RPossible -> IO RSig
rSig poss@(RPossible maxParamms nNamedSigs) = do
  x <- pick [ RSigFormula <$> rFormula poss
            , RSigGen <$> rGen poss
            , RSig <$> rSigName poss
            , RV <$> rParam poss
            ]
  x


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
               x


-- | = RSin
rSin :: RPossible -> IO RGen
rSin poss = RSin <$> rSinMsg poss

data RSinMsg = RSinMsg { rSinFreq :: RSig, rSinPhase :: RSig }
  deriving (Show, Eq, Ord)

rSinMsg :: RPossible -> IO RSinMsg -- TODO
rSinMsg poss = do a <- rSig poss
                  b <- rSig poss
                  return $ RSinMsg a b


-- | = RSaw
rSaw :: RPossible -> IO RGen
rSaw poss = RSaw <$> rSawMsg poss

data RSawMsg = RSawMsg { rSawFreq :: RSig }
  deriving (Show, Eq, Ord)

rSawMsg :: RPossible -> IO RSawMsg
rSawMsg poss = RSawMsg <$> rSig poss


-- | An `RSigName` identifies a previously named `RSig`. For instance,
-- in `s1 <- sinOsc (freq_ (V::V "freq"))`, we have created a signal
-- named `s1`. 
data RSigName = RS1 | RS2 | RS3 | RS4 | RS5 | RS6 | RS7 | RS8
  deriving (Show, Eq, Ord)

theRSigNames = [RS1, RS2, RS3, RS4, RS5, RS6, RS7, RS8]

rSigName :: RPossible -> IO RSigName
rSigName (RPossible _ maxSigName) = pick $ take maxSigName theRSigNames


-- | = Every random synth has up to eight parameters.
data RParam = RP1 | RP2 | RP3 | RP4 | RP5 | RP6 | RP7 | RP8
  deriving (Show, Eq, Ord)

theRParams = [RP1, RP2, RP3, RP4, RP5, RP6, RP7, RP8]

rParam :: RPossible -> IO RParam
rParam (RPossible maxParam _) = pick $ take maxParam theRParams
