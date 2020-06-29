module Montevideo.Monome.Util (
  ByteString, pack, unpack
  , fi         -- ^ (Integral a, Num b) => a -> b
  , fr         -- ^ Fractional a => Rational -> a
  , numBetween -- ^ (Num a, Ord a) => a -> a -> a -> Bool
  , dot        -- ^ Num a => (a,a) -> (a,a) -> a
  , taxiMetric -- ^ Num a => (a,a) -> (a,a) -> a
  , addPair    -- ^ Num a => (a,a) -> (a,a) -> (a,a)
  , mulPair    -- ^ Num a => a -> (a,a) -> (a,a)
  , negPair    -- ^ Num a => (a,a) -> (a,a)
  , uniq       -- ^ Ord a => [a] -> [a]
  )

where

import qualified Data.Set as S
import Data.ByteString (ByteString)
import Data.ByteString.Char8 (pack, unpack)


-- | Because OSC needs a lot of Int32 values while I prefer Int.
fi :: (Integral a, Num b) => a -> b
fi = fromIntegral

fr :: Fractional a => Rational -> a
fr = fromRational

numBetween :: (Num a, Ord a) => a -> a -> a -> Bool
numBetween low high x = x >= low && x <= high

dot :: Num a => (a,a) -> (a,a) -> a
dot (a,b) (c,d) = a*c + b*d

taxiMetric :: Num a => (a,a) -> (a,a) -> a
taxiMetric (a,b) (c,d) = abs (a-c) + abs (b-d)

addPair :: Num a => (a,a) -> (a,a) -> (a,a)
addPair (a,b) (c,d) = (a+c, b+d)

mulPair :: Num a => a -> (a,a) -> (a,a)
mulPair n (a,b) = (n*a,n*b)

negPair :: Num a => (a,a) -> (a,a)
negPair (a,b) = (-a,-b)

uniq :: Ord a => [a] -> [a]
uniq = S.toList . S.fromList
