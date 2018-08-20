{-# LANGUAGE
MultiParamTypeClasses
, FunctionalDependencies
, FlexibleInstances
, UndecidableInstances
#-}

module Vivid.Jbb.Dispatch.HasStart where

class HasStart h n | h -> n where
  fst' :: h -> n
  setFst' :: n -> h -> h
  overFst' :: (n -> n) -> h -> h

instance HasStart Rational Rational where
  fst' = id
  setFst' = const
  overFst' f h = f h

instance HasStart Float Float where
  fst' = id
  setFst' = const
  overFst' f h = f h

instance HasStart a n => HasStart (a,b) n where
  fst' (a,b) = fst' a
  setFst' r (a,b) = (setFst' r a, b)
  overFst' f (a,b) = (overFst' f a, b)
