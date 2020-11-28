module Montevideo.Util.Edo where

import Data.Fixed
import Data.Ratio


cents :: Floating a => Rational -> a
cents r = octavesToDents $ log (fromRational r) / log 2

octavesToDents :: Fractional a => a -> a
octavesToDents = (*) (10000 * 6 / 5)
