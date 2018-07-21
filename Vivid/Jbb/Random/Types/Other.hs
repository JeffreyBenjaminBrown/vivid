module Vivid.Jbb.Random.Types.Other where


-- | Without RandConstraints we would usually create invalid signal graphs
  -- For instance, we can't refer to the 5th named signal if there are only 4.
-- TODO ? This is really a hodgepodge of constraints (e.g. maxSignals)
  -- and state (e.g. namedSignals, which should not exceed maxSignals).
data RandConstraints = RandConstraints
  { nParams :: Int -- in [1,8]
  , namedSignals :: Int -- in [0,maxSignals]
  , maxSignals :: Int -- in [1,8]
  , depth :: Int -- in [1, maxDepth]
  , maxDepth :: Int -- greater than 1
  } deriving (Show, Eq)
