{-# LANGUAGE DataKinds #-}

module Montevideo.Random.Types.AbstractSignal where

import qualified Data.Map as M


type AbSynth = M.Map AbSigName AbSig

data AbSig = AbSigFormula AbFormula
           | AbSigGen AbGen
           | AbSig AbSigName -- ^ a previously constructed AbSig
           | AbV AbParam
           | AbConst Float
  deriving (Show, Eq, Ord)

data AbFormula = AbProd AbSig AbSig
               | AbSum AbSig AbSig
  deriving (Show, Eq, Ord)

data AbGen = AbSin AbSinMsg
           | AbSaw AbSawMsg
  deriving (Show, Eq, Ord)

data AbSinMsg = AbSinMsg { abSinFreq :: AbSig, abSinPhase :: AbSig }
  deriving (Show, Eq, Ord)

data AbSawMsg = AbSawMsg { abSawFreq :: AbSig }
  deriving (Show, Eq, Ord)

-- | Not every abstract signal has a name.
-- Those that do can be referred to by later signals.
data AbSigName = AS1 | AS2 | AS3 | AS4 | AS5 | AS6 | AS7 | AS8
  deriving (Show, Eq, Ord)

theAbSigNames :: [AbSigName]
theAbSigNames = [AS1, AS2, AS3, AS4, AS5, AS6, AS7, AS8]

-- | = Every random synth has up to eight parameters.
data AbParam = AP1 | AP2 | AP3 | AP4 | AP5 | AP6 | AP7 | AP8
  deriving (Show, Eq, Ord)

type TheAbParams = '["AP1", "AP2", "AP3", "AP4", "AP5", "AP6", "AP7", "AP8"]

theAbParams :: [AbParam]
theAbParams = [AP1, AP2, AP3, AP4, AP5, AP6, AP7, AP8]
