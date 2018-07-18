{-# LANGUAGE DataKinds, ExtendedDefaultRules, GADTs #-}

import Vivid
import Data.List as L
import Data.Map as M
import GHC.TypeLits


-- | = messages, which know where they're going

data Msg where
  ParamMsg :: (VarList usedParams, Subset (InnerVars usedParams) allParams)
      => Synth allParams -> usedParams -> Msg
  FreeMsg :: Synth params -> Msg

send :: VividAction m => Msg -> m ()
send (ParamMsg synth params) = set synth params
send (FreeMsg synth) = free synth


-- | = main

main = do
  b <- synth bop ()
  b' <- synth boop ()
  let m  = ParamMsg b  (toI 500 :: I "freq")
      m' = ParamMsg b' (toI 600 :: I "freq", toI 0.02 :: I "amp")
      fm = FreeMsg b
      fm' = FreeMsg b'
      messages = [m,m',fm,fm'] -- checking whether the compiler minds
  mapM_ send [m,m']
  wait 1
  mapM_ send [fm,fm']


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
