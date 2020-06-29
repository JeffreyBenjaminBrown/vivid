{-# LANGUAGE ViewPatterns, DataKinds #-}

module Montevideo.Random.RandomSignal where

import Vivid
import Random.Types


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

ranAbFormula :: RandConstraints -> IO AbFormula
ranAbFormula poss = do f <- pick [AbProd, AbSum]
                       a <- randAbSig poss
                       b <- randAbSig poss
                       return $ f a b

randAbGen :: RandConstraints -> IO AbGen
randAbGen poss = do x <- pick [ randAbSaw poss
                              , randAbSin poss ]
                    x

-- | = sinOsc abstraction
randAbSin :: RandConstraints -> IO AbGen
randAbSin poss = AbSin <$> randAbSinMsg poss

randAbSinMsg :: RandConstraints -> IO AbSinMsg
randAbSinMsg poss = do a <- randAbSig poss
                       b <- randAbSig poss
                       return $ AbSinMsg a b


-- | = AbSaw
randAbSaw :: RandConstraints -> IO AbGen
randAbSaw poss = AbSaw <$> randAbSawMsg poss

randAbSawMsg :: RandConstraints -> IO AbSawMsg
randAbSawMsg poss = AbSawMsg <$> randAbSig poss

randAbSigName :: RandConstraints -> IO AbSigName
randAbSigName cstrs =
  pick $ take (namedSignals cstrs) theAbSigNames

randAbParam :: RandConstraints -> IO AbParam
randAbParam cs = pick $ take (nParams cs) theAbParams
