module Montevideo.JI.Util where


fromCents :: Double -> Double
fromCents _cents = exp $ log 2 * dentsToOctaves _cents

-- | These should really be called "pence" -- 10ths of a cent.
cents :: Floating a => Rational -> a
cents r = stretch $ log (fromRational r) / log 2

-- | A "dent" is a tenth of a cent. Not sure why I did that.
dentsToOctaves :: Fractional a => a -> a
dentsToOctaves x = x / (10000 * 6 / 5)

-- | A "dent" is a tenth of a cent. Not sure why I did that.
stretch :: Fractional a => a -> a
stretch = (*) (10000 * 6 / 5)
