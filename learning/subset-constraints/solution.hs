-- | Why won't `main` compile?
-- I need a function that returns messages of different types
-- but all acceptable to a particular kind of `Synth`.

{-# LANGUAGE DataKinds
           , ExtendedDefaultRules
           , ScopedTypeVariables
           , GADTs #-}

import Vivid

boop :: SynthDef '["freq","amp"]
boop = sd ( (0,0.01) -- default values
            :: (I "freq",I "amp")) $ do
  s1 <- (V::V "amp") ~* sinOsc (freq_ (V::V "freq"))
  out 0 [s1, s1]

data Message sdArgs where
  Message :: forall params sdArgs.
             (VarList params
             , Subset (InnerVars params) sdArgs)
          => params -> Message sdArgs

message :: Int -> Message '["freq","amp"]
message 0 = Message (toI 444 :: I "freq")
message 1 = Message (toI 0.1 :: I "amp")
message _ = Message ()

main = do
  s <- synth boop ()
  case message 0 of
    Message a -> set s a
    _ -> return ()
  case message 1 of
    Message a -> set s a
    _ -> return ()
  wait 1
  freeAll
