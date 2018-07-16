{-# LANGUAGE DataKinds, ExtendedDefaultRules, GADTs #-}

data Aware a where
  Aware :: Int -> Aware a

instance Show a => Show (Aware a) where
  show (Aware a) = "Aware " ++ show a

xAdd :: Aware a -> Aware a -> Aware a
xAdd (Aware a) (Aware b) = Aware $ a + b

data Forgot where
  Forgot :: Aware a -> Forgot

-- Of course the following function doesn't compile:
-- How would we know whether a and b are the same type?
forgetAdd :: Forgot -> Forgot -> Forgot
forgetAdd (Forgot a) (Forgot b) = Forgot $ xAdd a b

-- I would like to be able to use forgetAdd on `Forgot`s
-- that were created from the same type of `Aware`, as in the following:
main = do
  let list =  [Forgot (Aware 1  :: Aware ()), Forgot (Aware 2  :: Aware (()))]
      list' = [Forgot (Aware 10 :: Aware ()), Forgot (Aware 20 :: Aware (()))]
      aSum = forgetAdd (head list) (head list')
  return ()

-- In Vivid I have a similar problem: I want to keep a collection of Synths,
-- and a collection of VarLists (note that VarList is a class, not a type),
-- and dispatch messages from the second collection to synths from the
-- first collection.
