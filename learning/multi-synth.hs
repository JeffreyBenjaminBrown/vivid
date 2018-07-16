-- there's a more interesting synth in param-as-arg.hs

{-# LANGUAGE DataKinds, ExtendedDefaultRules, GADTs #-}

import Vivid
import Data.List as L
import Data.Map as M
import GHC.TypeLits


-- | = messages, which know where they're going

data Msg where
  Msg :: (VarList usedParams, Subset (InnerVars usedParams) allParams)
      => Synth allParams -> usedParams -> Msg

send :: VividAction m => Msg -> m ()
send (Msg synth params) = set synth params

sendFree :: VividAction m => Msg -> m ()
sendFree (Msg synth _) = free synth


-- | = main

main = do
  b <- synth bop ()
  b' <- synth boop ()
  let m  = Msg b  (toI 500 :: I "freq")
      m' = Msg b' (toI 600 :: I "freq", toI 0.02 :: I "amp")
  mapM_ send [m,m']
  wait 1
  mapM_ sendFree [m,m']


-- | = synths

bop :: SynthDef '["freq"]
bop = sd ( 0 :: I "freq"
         ) $ do
   s1 <- 0.01 ~* sinOsc (freq_ (V::V "freq"))
   out 0 [s1, s1]

boop :: SynthDef '["freq","amp"]
boop = sd ( 0    :: I "freq"
          , 0.01 :: I "amp"
          ) $ do
   s1 <- (V::V "amp") ~* sinOsc (freq_ (V::V "freq"))
   out 0 [s1, s1]

