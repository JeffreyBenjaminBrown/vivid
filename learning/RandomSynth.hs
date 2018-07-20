{-# LANGUAGE ViewPatterns #-}

--module Vivid.Jbb.RandomSynth where
--
import Vivid


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

data RSig = RSig RSigName -- ^ a previously constructed RSig
          | RV RParam
          | RSigFormula RFormula
          | RSigGen RGen
  deriving (Show, Eq, Ord)

data RFormula = RProd RSig RSig
              | RSum RSig RSig
  deriving (Show, Eq, Ord)

data RGen = RSin RSinMsg
          | RSaw RSawMsg
  deriving (Show, Eq, Ord)

data RSinMsg = RSinMsg { rSinFreq :: RSig, rSinPhase :: RSig }
  deriving (Show, Eq, Ord)

data RSawMsg = RSawMsg { rSawFreq :: RSig }
  deriving (Show, Eq, Ord)


-- | = Generating random ones

-- | without this we would usually create invalid signal graphs
-- TODO ? How to ensure that previously created signals are actually used?
data RPossible = RPossible { maxParams :: Int
                           , nNamedSignals :: Int }

rSawMsg :: IO RSawMsg -- TODO
rSawMsg = return $ RSawMsg (RV RP1)

rSaw :: IO RGen
rSaw = RSaw <$> rSawMsg

rSinMsg :: IO RSinMsg -- TODO
rSinMsg = return $ RSinMsg (RV RP1) (RV RP2)

rSin :: IO RGen
rSin = RSin <$> rSinMsg

rGen :: IO RGen
rGen = do x <- pick [rSaw, rSin]
          x -- this doesn't return `x`, it "does" `x`

rFormula :: IO RFormula
rFormula = do f <- pick [RProd, RSum]
              a <- rSig
              b <- rSig
              return $ f a b

rSig :: IO RSig -- TODO
rSig = return $ RV RP1
