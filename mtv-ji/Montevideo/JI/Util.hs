module Montevideo.JI.Util where

import           Data.Ratio


fromCents :: Double -> Double
fromCents _cents = exp $ log 2 * unStretch _cents

-- | These should really be called "pence" -- 10ths of a cent.
cents :: Floating a => Rational -> a
cents r = stretch $ log (fromRational r) / log 2

unStretch :: Fractional a => a -> a
unStretch x = x / (10000 * 6 / 5)

stretch :: Fractional a => a -> a
stretch = (*) (10000 * 6 / 5)

edo :: Integral i => i -> [Rational]
edo n = [ fromIntegral d % fromIntegral n
        | d <- [0..n-1] ]
