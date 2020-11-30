module Montevideo.Util.Edo where


type Edo = Int -- ^ e.g. 12 for "normal" music, 31 for even better music ...

-- | Really this manipulates dents (see below), not cents.
fromCents :: Double -> Double
fromCents _cents = exp $ log 2 * dentsToOctaves _cents

-- | Really this manipulates dents (see below), not cents.
cents :: Floating a => Rational -> a
cents r = octavesToDents $ log (fromRational r) / log 2

-- | A "dent" is a tenth of a cent. Not sure why I did that.
dentsToOctaves :: Fractional a => a -> a
dentsToOctaves x = x / (10000 * 6 / 5)

-- | A "dent" is a tenth of a cent. Not sure why I did that.
octavesToDents :: Fractional a => a -> a
octavesToDents = (*) (10000 * 6 / 5)
