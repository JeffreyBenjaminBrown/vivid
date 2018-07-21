{-# LANGUAGE ViewPatterns #-}

module Vivid.Jbb.Random.Signal where

import Vivid


-- | Without RandConstraints we would usually create invalid signal graphs
  -- For instance, we can't refer to the 5th named signal if there are only 4.
-- TODO ? This is really a hodgepodge of constraints (e.g. maxSignals)
  -- and state (e.g. namedSignals, which should not exceed maxSignals).
data RandConstraints = RandConstraints
  { nParams :: Int -- in [1,8]
  , namedSignals :: Int -- in [0,maxSignals]
  , maxSignals :: Int -- in [1,8]
  , depth :: Int -- in [1, maxDepth]
  , maxDepth :: Int -- greater than 1
  } deriving (Show, Eq)


-- | = abstract signal
-- An "AbX" is a representation of an X suitable for creating an X.
data AbSig = AbSigFormula AbFormula
           | AbSigGen AbGen
           | AbSig AbSigName -- ^ a previously constructed AbSig
           | AbV AbParam
  deriving (Show, Eq, Ord)

-- | This implements a maximum depth constraint and a minimum complexity one.
-- The latter constraint is that a signal with a depth of 1 cannot be a or
-- the name of another signal, because then it would just be a synonym.
randAbSig :: RandConstraints -> IO AbSig
randAbSig poss = do x <- pick $ maybeAParam ++ maybeGoDeeper ++ maybeAName
                    x
  where poss' = poss {depth = depth poss + 1}
        maybeAParam = if depth poss > 1
          then [AbV <$> randAbParam poss'] else []
        maybeGoDeeper = if depth poss < maxDepth poss
          then [ AbSigFormula <$> ranAbFormula poss'
               , AbSigGen <$> randAbGen poss' ]
          else []
        maybeAName = if depth poss > 1 && namedSignals poss > 0
          then [AbSig <$> randAbSigName poss'] else []


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
randAbGen poss = do x <- pick [ randAbSaw poss
                              , randAbSin poss ]
                    x


-- | = sinOsc abstraction
randAbSin :: RandConstraints -> IO AbGen
randAbSin poss = AbSin <$> randAbSinMsg poss

data AbSinMsg = AbSinMsg { abSinFreq :: AbSig, abSinPhase :: AbSig }
  deriving (Show, Eq, Ord)

randAbSinMsg :: RandConstraints -> IO AbSinMsg
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

sigName :: RandConstraints -> Int -> AbSigName
sigName (namedSignals -> k) n =
  if n > 0 && n <= min k 8
  then theAbSigNames !! (n - 1)
  else error $ show n ++ " is not the number of an AbSigName."

randAbSigName :: RandConstraints -> IO AbSigName
randAbSigName cstrs =
  pick $ take (namedSignals cstrs) theAbSigNames


-- | = Every random synth has up to eight parameters.
data AbParam = AP1 | AP2 | AP3 | AP4 | AP5 | AP6 | AP7 | AP8
  deriving (Show, Eq, Ord)

theRParams = [AP1, AP2, AP3, AP4, AP5, AP6, AP7, AP8]

randAbParam :: RandConstraints -> IO AbParam
randAbParam cs = pick $ take (nParams cs) theRParams
