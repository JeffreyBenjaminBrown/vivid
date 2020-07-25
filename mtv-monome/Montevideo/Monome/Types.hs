module Montevideo.Monome.Types
  ( module Montevideo.Monome.Types.Most
  -- The instances defined in Montevideo.Monome.Types.Instances
  -- are all for types defined in Montevideo.Monome.Types.Most,
  -- so once Montevideo.Monome.Types.Most is exported,
  -- there is no need to export Montevideo.Monome.Types.Instances,
  -- and in fact doing so elicits a warning from GHC.
  ) where

import Montevideo.Monome.Types.Most
import Montevideo.Monome.Types.Instances()
