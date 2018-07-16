{-# LANGUAGE MultiParamTypeClasses
           , AllowAmbiguousTypes
           , FlexibleInstances
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

data MessageA format where
  MessageA :: String -> MessageA format
data MessageB format where
  MessageB :: String -> MessageB format

class (Message format) a where
  theMessage :: a -> String
instance (Message format) (MessageA format) where
  theMessage (MessageA msg) = msg
instance (Message format) (MessageB format) where
  theMessage (MessageB msg) = msg

-- | Argh why won't `play` compile?
play :: Message format m => Synth format -> m -> IO ()
play (Synth name) msg =
  print $ name ++ " now sounds like " ++ theMessage msg

-- | After that, can we define the following?
--   (1) a heterogeneous list of `Synth x`s
--     where `x` varies across the members of the list.
--   (2) a heterogeneous list of `msg x`s where `x` varies across members
--     of the list and where `msg` can be either `MessageA` or `MessageB`.
--   (3) a function that lets you send messages from the second list
--     to synths from the first list.
