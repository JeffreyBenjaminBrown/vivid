{-# LANGUAGE MultiParamTypeClasses
           , FlexibleInstances
           , ScopedTypeVariables
           , GADTs #-}

-- | In Vivid, a `Synth` will accept any member of the `VarList` type
-- family, as long as the parameters of the `VarList` are a subset of
-- the parameters of the `Synth`:
-- set :: ( VividAction m
--        , Subset (InnerVars params) sdArgs
--        , VarList params)
--     => Synth sdArgs -> params -> m ()

-- | Below is a plain text impression of that situation.
-- Things of type `Synth x` accept messages of
-- type `MessageA y` or `MessageB y`, but only if `x == y`.

type SynthName = String

data Synth format where
  Synth :: SynthName -> Synth format

data Message format where
  Message :: String -> Message format
data PreMessageA format where
  PreMessageA :: String -> PreMessageA format
data PreMessageB format where
  PreMessageB :: String -> PreMessageB format

class Messageable format a where
  toMessage :: a -> Message format
instance Messageable format (PreMessageA format) where
  toMessage (PreMessageA msg) = Message msg
instance Messageable format (PreMessageB format) where
  toMessage (PreMessageB msg) = Message msg

play :: forall format m
     .  Messageable format m
     => Synth format -> m -> IO ()
play (Synth name) msg =
  let Message m = toMessage msg :: Message format
  in print $ name ++ " now sounds like " ++ m

-- | After that, can we define the following?
--   (1) a heterogeneous list of `Synth x`s
--     where `x` varies across the members of the list.
--   (2) a heterogeneous list of `msg x`s where `x` varies across members
--     of the list and where `msg` can be either `MessageA` or `MessageB`.
--   (3) a function that lets you send messages from the second list
--     to synths from the first list.
