{-# LANGUAGE ScopedTypeVariables #-}

module Montevideo.Dispatch.Abbrevs (
    viewDurs -- ^ Museq l a -> Museq l a

  -- | abbreviations
  , j   -- ^ Just
  , n   -- ^ Nothing
  , m1  -- ^ M.singleton
  , mfl -- ^ M.fromList

  , stepsToHz       -- ^ Edo -> Float     -> Museq l ScParams -> Museq l ScParams
  , amp, freq       -- ^ (Float -> Float) -> Museq String ScParams -> Museq String ScParams
  , ampTo, freqTo   -- ^ Float            -> Museq String ScParams -> Museq String ScParams
  , nAmp, nFreq     -- ^ (Float -> Float) -> Museq String Note -> Museq String Note
  , nAmpTo, nFreqTo -- ^ Float            -> Museq String ScParams -> Museq String ScParams

  , mm    -- ^ RDuration -> [(l,RTime,RTime,a)] -> Museq l a
  , mmh   -- ^ forall a l. Ord l =>
      -- RDuration -> [(l,RDuration,a)] -> Museq l a
  , mmhm  -- ^ forall a l. Ord l =>
      -- RDuration -> [(l, RTime, Maybe a)] -> Museq l a
  , mm1   -- ^ ScParams -> Museq String ScParams
  , mmho  -- ^ forall l. Ord l =>
      -- RDuration -> [(l,RDuration,ScParams)] -> Museq l ScParams
  , mmt  -- ^ forall l. (Ord l, Show l) =>
      -- RDuration -> [(l,RTime,Sample,ScParams)] -> Museq String Note
  , mmt1 -- ^ RDuration -> [(RTime,Sample)] -> Museq String Note
  , ons   -- ^ Museq l ScParams -> Museq l ScParams
  , ops   -- ^ [(ParamName, Float -> Float)]
          -- -> Museq l ScParams -> Museq l ScParams
  , nBoop, nVap, nZot, nSqfm -- ^ Museq l ScParams -> Museq l Note

  -- | = To write names only once
  , prefixPair, pre2   -- ^ a -> [(b,c)] -> [(a,b,c)]
  , prefixTriple, pre3 -- ^ a -> [(b,c,d)] -> [(a,b,c,d)]
  , pair               -- ^ a -> b -> (a,b)
  , trip               -- ^ a -> b -> c -> (a,b,c)
  , ev4                -- ^ l -> t -> t -> a -> Event t l a
  , ev4'               -- ^ (l, t, t, a) -> Event t l a
  ) where

import           Control.Lens
import qualified Data.Map as M

import Montevideo.Dispatch.Join
import Montevideo.Dispatch.Museq.Mk
import Montevideo.Dispatch.Transform
import Montevideo.Dispatch.Types
import Montevideo.Synth
import Montevideo.Synth.Msg
import Montevideo.Synth.Samples
import Montevideo.Util


viewDurs :: Museq l a -> Museq l a
viewDurs = vec .~ mempty


-- | = Abbreviations

j :: a -> Maybe a
j = Just

n :: Maybe a
n = Nothing

m1 :: k -> a -> M.Map k a
m1 = M.singleton

mfl :: Ord k => [(k, a)] -> M.Map k a
mfl = M.fromList

stepsToHz :: Edo -> Float -> Museq l ScParams -> Museq l ScParams
stepsToHz edo anchorInHz =
  freq $ (*) anchorInHz . (\p -> 2**(p/fi edo))

amp, freq :: (Float -> Float) -> Museq l ScParams -> Museq l ScParams
amp g = fmap $ M.adjust g "amp"
freq g = fmap $ M.adjust g "freq"

ampTo, freqTo :: Float -> Museq String ScParams -> Museq String ScParams
ampTo g = fmap $ M.insert "amp" g
freqTo g = fmap $ M.insert "freq" g

nAmp, nFreq :: (Float -> Float) -> Museq String Note -> Museq String Note
nAmp g  = fmap $ noteScParams %~ M.adjust g "amp"
nFreq g = fmap $ noteScParams %~ M.adjust g "freq"

nAmpTo, nFreqTo :: Float -> Museq String Note -> Museq String Note
nAmpTo g  = fmap $ noteScParams %~ M.insert "amp" g
nFreqTo g = fmap $ noteScParams %~ M.insert "freq" g

mm :: RDuration -> [(l,RTime,RTime,a)] -> Museq l a
mm = mkMuseq

mmh :: forall a l. Ord l => RDuration -> [(l,RDuration,a)] -> Museq l a
mmh = mkMuseqH

mmhm :: forall a l. Ord l
     => RDuration -> [(l, RTime, Maybe a)] -> Museq l a
mmhm = mkMuseqHm

mm1 :: ScParams -> Museq String ScParams
mm1 = mkMuseqOneScParams

mmho :: forall l. Ord l => RDuration -> [(l,RDuration,ScParams)] -> Museq l ScParams
mmho = mkMuseqHo

mmt :: forall l. (Ord l, Show l) =>
  RDuration -> [(l,RTime,Sample,ScParams)] -> Museq String Note
mmt = mkMuseqTrig

mmt1 :: RDuration -> [(RTime,Sample)] -> Museq String Note
mmt1 = mkMuseqTrig1

ons :: Museq l ScParams -> Museq l ScParams
ons = insertOns

ops :: [(ParamName, Float -> Float)] -> Museq l ScParams -> Museq l ScParams
ops = overParams

nBoop, nVap, nZot, nSqfm :: Museq l ScParams -> Museq l Note
nBoop = (<$>) (Note Boop)
nVap  = (<$>) (Note Vap)
nZot  = (<$>) (Note Zot)
nSqfm = (<$>) (Note Sqfm)


-- | = To write names only once

-- | `prefixPair` lets you write the intended voice for a collection
-- of events only once, if in fact they all should go to the same synth.
prefixPair, pre2 :: a -> [(b,c)] -> [(a,b,c)]
prefixPair a bcs = map (\(b,c) -> (a,b,c)) bcs
pre2 = prefixPair

prefixTriple, pre3 :: a -> [(b,c,d)] -> [(a,b,c,d)]
prefixTriple a x = map (\(b,c,d) -> (a,b,c,d)) x
pre3 = prefixTriple

pair :: a -> b -> (a,b)
pair = (,)

trip :: a -> b -> c -> (a,b,c)
trip a b c = (a,b,c)

ev4 :: l -> t -> t -> a -> Event t l a
ev4 l start end a = Event l (start,end) a

ev4' :: (l, t, t, a) -> Event t l a
ev4' (l,t,s,a) = ev4 l t s a
