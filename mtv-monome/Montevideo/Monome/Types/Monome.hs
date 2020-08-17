module Montevideo.Monome.Types.Monome
  ( HostName, Socket
  , X, Y, Switch, Led
  ) where

import qualified Network.Socket as NS


type HostName = NS.HostName
type Socket = NS.Socket

-- | X and Y are coordinates on the monome.
-- PITFALL: X rises from left to right, but Y rises from top to bottom.
-- Thus (0,1) is just under the top-left corner.
-- PITFALL: The monome will respond to out-of-bounds (x,y) values.
-- I don't use that feature.
type X = Int
type Y = Int

type Switch = Bool -- | Whether a monome button is pressed.
type Led    = Bool -- | Whether a monome LED is lit.
