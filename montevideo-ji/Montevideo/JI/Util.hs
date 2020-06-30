module Montevideo.JI.Util where

import           Data.Ratio


cents :: Floating a => Rational -> a
cents r = stretch $ log (fromRational r) / log 2

stretch :: Fractional a => a -> a
stretch = (*) (10000 * 6 / 5)

edo :: Integral i => i -> [Rational]
edo n = [ fromIntegral d % fromIntegral n
        | d <- [0..n-1] ]
