-- Main idea: set' takes a param(string) and a number,
-- rather than a value of type `I s`. Thus
-- it can send a value to a target param selected by the computer at runtime,
-- rather than requiring the programmer to choose that param at compile time.

{-# LANGUAGE DataKinds #-}

import Vivid
import Data.List as L
import Data.Map as M
import GHC.TypeLits


type MyParams = '["freq", "amp", "width", "width_vib"]

set' :: (Subset MyParams sdArgs
        , Real n, VividAction m)
     => String -> Node sdArgs -> n -> m ()
set' "freq"  s n = set s (toI n :: I "freq")
set' "amp"  s n = set s (toI n :: I "amp" )

boop :: SynthDef MyParams
boop = sd ( 0    :: I "freq"
          , 0.01 :: I "amp"
  -- the next two params do nothing, but are needed so that boop has
  -- the same interface as boop', so that both can be used with set'
          , 0    :: I "width"
          , 0    :: I "width_vib"
          ) $ do
   s1 <- (V::V "amp") ~* sinOsc (freq_ (V::V "freq"))
   out 0 [s1, s1]

boop' :: SynthDef MyParams
boop' = sd ( 0 :: I "freq"
           , 0.1 :: I "amp"
           , 50 :: I "width"
           , 51 :: I "width_vib"
           ) $ do
  s0 <- sinOsc (freq_ (V::V "width_vib"))
  s1 <- sinOsc (freq_ (V::V "width")) ~+ s0
  s2 <- (V::V "amp") ~* pulse (freq_  (V::V "freq"), width_ s1)
  out 0 [s2, s2]
