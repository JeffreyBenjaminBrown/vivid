module Montevideo.Util.Edo where


type Edo = Int -- ^ e.g. 12 for "normal" music, 31 for even better music ...

cents :: Floating a => Rational -> a
cents r = octavesToDents $ log (fromRational r) / log 2

octavesToDents :: Fractional a => a -> a
octavesToDents = (*) (10000 * 6 / 5)
