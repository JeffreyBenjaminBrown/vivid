module Montevideo.Monome.Types
  ( module Montevideo.Monome.Types.Initial
  -- The instances defined in Montevideo.Monome.Types.Instances
  -- are all for types defined in Montevideo.Monome.Types.Initial,
  -- so once Montevideo.Monome.Types.Initial is exported,
  -- there is no need to export Montevideo.Monome.Types.Instances,
  -- and in fact doing so elicits a warning from GHC.
  ) where

import Montevideo.Monome.Types.Initial
import Montevideo.Monome.Types.Instances()
