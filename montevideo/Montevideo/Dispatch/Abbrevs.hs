{-# LANGUAGE ScopedTypeVariables #-}

module Montevideo.Dispatch.Abbrevs (
    viewDurs -- ^ Museq l a -> Museq l a

  -- | abbreviations
  , j   -- ^ Just
  , n   -- ^ Nothing
  , m1  -- ^ M.singleton
  , mfl -- ^ M.fromList

  , hsToHz          -- ^ Float            -> Museq l Msg -> Museq l Msg
  , amp, freq       -- ^ (Float -> Float) -> Museq String Msg -> Museq String Msg
  , ampTo, freqTo   -- ^ Float            -> Museq String Msg -> Museq String Msg
  , nAmp, nFreq     -- ^ (Float -> Float) -> Museq String Note -> Museq String Note
  , nAmpTo, nFreqTo -- ^ Float            -> Museq String Msg -> Museq String Msg

  , mm    -- ^ RDuration -> [(l,RTime,RTime,a)] -> Museq l a
  , mmh   -- ^ forall a l. Ord l =>
      -- RDuration -> [(l,RDuration,a)] -> Museq l a
  , mmhm  -- ^ forall a l. Ord l =>
      -- RDuration -> [(l, RTime, Maybe a)] -> Museq l a
  , mm1   -- ^ Msg -> Museq String Msg
  , mmho  -- ^ forall l. Ord l =>
      -- RDuration -> [(l,RDuration,Msg)] -> Museq l Msg
  , mmrt  -- ^ forall l. (Ord l, Show l) =>
      -- RDuration -> [(l,RTime,Sample,Msg)] -> Museq String Note
  , mmrt1 -- ^ RDuration -> [(RTime,Sample)] -> Museq String Note
  , offs  -- ^ Museq l Msg -> Museq l Msg
  , ons   -- ^ Museq l Msg -> Museq l Msg
  , ops   -- ^ [(ParamName, Float -> Float)]
          -- -> Museq l Msg -> Museq l Msg
  , nBoop, nVap, nZot, nSqfm -- ^ Museq l Msg -> Museq l Note

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
import Montevideo.Dispatch.Museq
import Montevideo.Dispatch.Transform
import Montevideo.Dispatch.Types
import Montevideo.Synth
import Montevideo.Synth.Samples


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

hsToHz :: Float -> Museq l Msg -> Museq l Msg
hsToHz fundamental =
  freq $ (*) 300 . (\p -> 2**(p/12))

amp, freq :: (Float -> Float) -> Museq l Msg -> Museq l Msg
amp g = fmap $ M.adjust g "amp"
freq g = fmap $ M.adjust g "freq"

ampTo, freqTo :: Float -> Museq String Msg -> Museq String Msg
ampTo g = fmap $ M.insert "amp" g
freqTo g = fmap $ M.insert "freq" g

nAmp, nFreq :: (Float -> Float) -> Museq String Note -> Museq String Note
nAmp g  = fmap $ noteMsg %~ M.adjust g "amp"
nFreq g = fmap $ noteMsg %~ M.adjust g "freq"

nAmpTo, nFreqTo :: Float -> Museq String Note -> Museq String Note
nAmpTo g  = fmap $ noteMsg %~ M.insert "amp" g
nFreqTo g = fmap $ noteMsg %~ M.insert "freq" g

mm :: RDuration -> [(l,RTime,RTime,a)] -> Museq l a
mm = mkMuseq

mmh :: forall a l. Ord l => RDuration -> [(l,RDuration,a)] -> Museq l a
mmh = mkMuseqH

mmhm :: forall a l. Ord l
     => RDuration -> [(l, RTime, Maybe a)] -> Museq l a
mmhm = mkMuseqHm

mm1 :: Msg -> Museq String Msg
mm1 = mkMuseqOneMsg

mmho :: forall l. Ord l => RDuration -> [(l,RDuration,Msg)] -> Museq l Msg
mmho = mkMuseqHo

mmrt :: forall l. (Ord l, Show l) =>
  RDuration -> [(l,RTime,Sample,Msg)] -> Museq String Note
mmrt = mkMuseqRt

mmrt1 :: RDuration -> [(RTime,Sample)] -> Museq String Note
mmrt1 = mkMuseqRt1

offs :: Museq l Msg -> Museq l Msg
offs = insertOffs

ons :: Museq l Msg -> Museq l Msg
ons = insertOns

ops :: [(ParamName, Float -> Float)] -> Museq l Msg -> Museq l Msg
ops = overParams

nBoop, nVap, nZot, nSqfm :: Museq l Msg -> Museq l Note
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
