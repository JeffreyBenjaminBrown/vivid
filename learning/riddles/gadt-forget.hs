{-# LANGUAGE DataKinds, ExtendedDefaultRules, GADTs #-}

data X a where
  X :: Int -> X a

instance Show a => Show (X a) where
  show (X a) = "X " ++ show a

xAdd :: X a -> X a -> X a
xAdd (X a) (X b) = X $ a + b

data Forget where
  Forget :: X a -> Forget

-- Of course the following function doesn't compile:
-- How would we know whether a and b are the same type?
forgetAdd :: Forget -> Forget -> Forget
forgetAdd (Forget a) (Forget b) = Forget $ xAdd a b

-- I would like to be able to use forgetAdd on `Forget`s
-- that were created from the same type of `X`, as in the following:
main = do
  let list =  [Forget (X 1  :: X ()), Forget (X 2  :: X (()))]
      list' = [Forget (X 10 :: X ()), Forget (X 20 :: X (()))]
      aSum = forgetAdd (head list) (head list')
  return ()

-- In Vivid I have a similar problem: I want to keep a collection of Synths,
-- and a collection of VarLists (note that VarList is a class, not a type),
-- and dispatch messages from the second collection to synths from the
-- first collection.
