{-# LANGUAGE ViewPatterns #-}

module Vivid.Jbb.Random.Signal where

import Vivid


-- | Without RandPossib we would usually create invalid signal graphs
-- For instance, we can't refer to the 5th named signal if there are only 4.
-- TODO ? How to ensure that previously created signals are actually used?
data RandPossib = RandPossib { maxParams :: Int
                             , nNamedSignals :: Int }


-- | = abstract signal
-- An "AbX" is a representation of an X suitable for creating an X.
data AbSig = AbSigFormula AbFormula
           | AbSigGen AbGen
           | AbSig AbSigName -- ^ a previously constructed AbSig
           | AbV AbParam
  deriving (Show, Eq, Ord)

randAbSig :: RandPossib -> IO AbSig
randAbSig poss@(RandPossib maxParamms nNamedSigs) = do
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

ranAbFormula :: RandPossib -> IO AbFormula
ranAbFormula poss = do f <- pick [RProd, RSum]
                       a <- randAbSig poss
                       b <- randAbSig poss
                       return $ f a b


-- | = abstract generator
data AbGen = AbSin AbSinMsg
           | AbSaw AbSawMsg
  deriving (Show, Eq, Ord)

randAbGen :: RandPossib -> IO AbGen
randAbGen poss = do x <- pick [randAbSaw poss, randAbSin poss]
                    x


-- | = sinOsc abstraction
randAbSin :: RandPossib -> IO AbGen
randAbSin poss = AbSin <$> randAbSinMsg poss

data AbSinMsg = AbSinMsg { abSinFreq :: AbSig, abSinPhase :: AbSig }
  deriving (Show, Eq, Ord)

randAbSinMsg :: RandPossib -> IO AbSinMsg -- TODO
randAbSinMsg poss = do a <- randAbSig poss
                       b <- randAbSig poss
                       return $ AbSinMsg a b


-- | = AbSaw
randAbSaw :: RandPossib -> IO AbGen
randAbSaw poss = AbSaw <$> randAbSawMsg poss

data AbSawMsg = AbSawMsg { abSawFreq :: AbSig }
  deriving (Show, Eq, Ord)

randAbSawMsg :: RandPossib -> IO AbSawMsg
randAbSawMsg poss = AbSawMsg <$> randAbSig poss


-- | An `AbSigName` identifies a previously named `AbSig`. For instance,
-- in `s1 <- sinOsc (freq_ (V::V "freq"))`, we have created a signal
-- named `s1`. 
data AbSigName = AS1 | AS2 | AS3 | AS4 | AS5 | AS6 | AS7 | AS8
  deriving (Show, Eq, Ord)

theAbSigNames = [AS1, AS2, AS3, AS4, AS5, AS6, AS7, AS8]

randAbSigName :: RandPossib -> IO AbSigName
randAbSigName (RandPossib _ maxSigName) =
  pick $ take maxSigName theAbSigNames


-- | = Every random synth has up to eight parameters.
data AbParam = AP1 | AP2 | AP3 | AP4 | AP5 | AP6 | AP7 | AP8
  deriving (Show, Eq, Ord)

theRParams = [AP1, AP2, AP3, AP4, AP5, AP6, AP7, AP8]

randAbParam :: RandPossib -> IO AbParam
randAbParam (RandPossib maxParam _) = pick $ take maxParam theRParams
