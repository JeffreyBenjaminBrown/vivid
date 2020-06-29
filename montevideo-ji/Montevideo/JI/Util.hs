module Montevideo.JI.Util where

import           Control.Lens
import qualified Data.List as L
import qualified Data.Set as S
import           Data.Decimal
import           Data.Ratio
import           Data.Fixed
import           Data.Ord (comparing)


cents :: Floating a => Rational -> a
cents r = stretch $ log (fromRational r) / log 2

stretch :: Fractional a => a -> a
stretch = (*) (10000 * 6 / 5)

edo :: Integral i => i -> [Rational]
edo n = [ fromIntegral d % fromIntegral n
        | d <- [0..n-1] ]
