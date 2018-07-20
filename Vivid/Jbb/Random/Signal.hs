{-# LANGUAGE ViewPatterns #-}

module Vivid.Jbb.Random.Signal where

import Vivid


-- | Without RandConstraints we would usually create invalid signal graphs
-- For instance, we can't refer to the 5th named signal if there are only 4.
-- TODO ? How to ensure that previously created signals are actually used?
data RandConstraints = RandConstraints
  { nParams :: Int
  , signalsNamedSoFar :: Int
  , maxSignals :: Int
  , maxSignalComplexity :: Int
  }


-- | = abstract signal
-- An "AbX" is a representation of an X suitable for creating an X.
data AbSig = AbSigFormula AbFormula
           | AbSigGen AbGen
           | AbSig AbSigName -- ^ a previously constructed AbSig
           | AbV AbParam
  deriving (Show, Eq, Ord)

randAbSig :: RandConstraints -> IO AbSig
randAbSig poss = do
  x <- pick [ AbSigFormula <$> ranAbFormula poss
            , AbSigGen <$> randAbGen poss
            , AbSig <$> randAbSigName poss
            , AbV <$> randAbParam poss
            ]
  x


-- | = abstract formula
data AbFormula = RProd AbSig AbSig
              | RSum AbSig AbSig
  deriving (Show, Eq, Ord)

ranAbFormula :: RandConstraints -> IO AbFormula
ranAbFormula poss = do f <- pick [RProd, RSum]
                       a <- randAbSig poss
                       b <- randAbSig poss
                       return $ f a b


-- | = abstract generator
data AbGen = AbSin AbSinMsg
           | AbSaw AbSawMsg
  deriving (Show, Eq, Ord)

randAbGen :: RandConstraints -> IO AbGen
randAbGen poss = do x <- pick [randAbSaw poss, randAbSin poss]
                    x


-- | = sinOsc abstraction
randAbSin :: RandConstraints -> IO AbGen
randAbSin poss = AbSin <$> randAbSinMsg poss

data AbSinMsg = AbSinMsg { abSinFreq :: AbSig, abSinPhase :: AbSig }
  deriving (Show, Eq, Ord)

randAbSinMsg :: RandConstraints -> IO AbSinMsg -- TODO
randAbSinMsg poss = do a <- randAbSig poss
                       b <- randAbSig poss
                       return $ AbSinMsg a b


-- | = AbSaw
randAbSaw :: RandConstraints -> IO AbGen
randAbSaw poss = AbSaw <$> randAbSawMsg poss

data AbSawMsg = AbSawMsg { abSawFreq :: AbSig }
  deriving (Show, Eq, Ord)

randAbSawMsg :: RandConstraints -> IO AbSawMsg
randAbSawMsg poss = AbSawMsg <$> randAbSig poss


-- | An `AbSigName` identifies a previously named `AbSig`. For instance,
-- in `s1 <- sinOsc (freq_ (V::V "freq"))`, we have created a signal
-- named `s1`.
data AbSigName = AS1 | AS2 | AS3 | AS4 | AS5 | AS6 | AS7 | AS8
  deriving (Show, Eq, Ord)

theAbSigNames = [AS1, AS2, AS3, AS4, AS5, AS6, AS7, AS8]

sigName :: Int -> Int -> AbSigName
sigName maxAbSigName n = if n <= min maxAbSigName 8 && n >= 1
  then theAbSigNames !! (n-1)
  else error $ show n ++ " is not the number of an AbSigName."

randAbSigName :: RandConstraints -> IO AbSigName
randAbSigName cstrs =
  pick $ take (signalsNamedSoFar cstrs) theAbSigNames


-- | = Every random synth has up to eight parameters.
data AbParam = AP1 | AP2 | AP3 | AP4 | AP5 | AP6 | AP7 | AP8
  deriving (Show, Eq, Ord)

theRParams = [AP1, AP2, AP3, AP4, AP5, AP6, AP7, AP8]

randAbParam :: RandConstraints -> IO AbParam
randAbParam cs = pick $ take (nParams cs) theRParams
