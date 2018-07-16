{-# LANGUAGE DataKinds, ExtendedDefaultRules, GADTs #-}

data X a where
  X :: Int -> X a

instance Show a => Show (X a) where
  show (X a) = "X " ++ show a

xAdd :: X a -> X a -> X a
xAdd (X a) (X b) = X $ a + b

data Forget where
  Forget :: X a -> Forget

forgetAdd :: Forget -> Forget -> Forget
forgetAdd (Forget a) (Forget b) = Forget $ xAdd a b
