-- the type families here are duplicated from Vivid/SynthDef/TypesafeArgs.hs

{-# LANGUAGE TypeOperators
           , TypeFamilies
           , MultiParamTypeClasses
           , DataKinds
           , GADTs #-}

import GHC.TypeLits
import GHC.Exts (Constraint)

type family Elem (a :: Symbol) (xs :: [Symbol]) :: Constraint where
   Elem a (a ': xs) = ()
   Elem a (x ': xs) = Elem a xs

type family Subset (as :: [Symbol]) (bs :: [Symbol]) :: Constraint where
   Subset '[] bs = ()
   Subset (a ': as) bs = (Elem a bs, Subset as bs)

data X phantoms where
  X :: String -> X phantoms

instance Show (X phantoms) where
  show (X string) = string
