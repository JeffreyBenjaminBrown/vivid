{-# LANGUAGE ScopedTypeVariables #-}

module Vivid.Jbb.Dispatch.Abbrevs where

import Vivid.Jbb.Dispatch.Museq
import Vivid.Jbb.Dispatch.Types


-- | = Abbreviations

mmh :: forall a l. Ord l => RDuration -> [(l,RDuration,a)] -> Museq' l a
mmh = mkMuseq'h

mmho :: forall l. Ord l => RDuration -> [(l,RDuration,Msg)] -> Museq' l Msg
mmho = mkMuseq'ho

offs :: Museq' l Msg -> Museq' l Msg
offs = insertOffs

ons :: Museq' l Msg -> Museq' l Msg
ons = insertOns


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
