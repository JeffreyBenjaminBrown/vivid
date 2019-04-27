{-# LANGUAGE
MultiParamTypeClasses
, FunctionalDependencies
, FlexibleInstances
, UndecidableInstances
#-}

module Vivid.Dispatch.HasStart where

import Control.Lens

class HasStart h n | h -> n where
  start :: Lens' h n

instance HasStart Rational Rational where
  start = id

instance HasStart Float Float where
  start = id

instance HasStart a n => HasStart (a,b) n where
  start = _1 . start
