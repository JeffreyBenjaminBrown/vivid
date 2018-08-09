module Vivid.Jbb.Distrib.Transform where

import Control.Lens (over, _1)
import qualified Data.Vector as V

import Vivid
import Vivid.Jbb.Distrib.Types


append :: Museq a -> Museq a -> Museq a
append a b = let d  = _dur a + _dur b
                 va = V.map (over _1 f) $ _vec a where
                   f time = time * (_dur a / d)
                 vb = V.map (over _1 f) $ _vec b where
                   f time = time * (_dur b / d) + (_dur a / d)
             in Museq {_dur = d, _vec = (V.++) va vb}
