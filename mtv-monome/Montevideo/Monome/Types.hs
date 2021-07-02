module Montevideo.Monome.Types
  ( module Montevideo.Monome.Types.Device
  , module Montevideo.Monome.Types.Edo

  -- The instances defined in Montevideo.Monome.Types.Instances
  -- are all for types defined in Montevideo.Monome.Types.Most,
  -- so once Montevideo.Monome.Types.Most is exported,
  -- there is no need to export Montevideo.Monome.Types.Instances,
  -- and in fact doing so elicits a warning from GHC.
  -- module Montevideo.Monome.Types.Instances

  , module Montevideo.Monome.Types.Monome
  , module Montevideo.Monome.Types.Most
  , module Montevideo.Monome.Types.Params
  ) where

import Montevideo.Monome.Types.Device
import Montevideo.Monome.Types.Edo
import Montevideo.Monome.Types.Instances()
import Montevideo.Monome.Types.Monome
import Montevideo.Monome.Types.Most
import Montevideo.Monome.Types.Params
