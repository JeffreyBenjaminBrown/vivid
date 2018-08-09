module Vivid.Jbb.Distrib.Transform where

import Control.Lens (over, _1)
import qualified Data.Vector as V

import Vivid
import Vivid.Jbb.Util
import Vivid.Jbb.Distrib.Museq (sortMuseq)
import Vivid.Jbb.Distrib.Types


append :: Museq a -> Museq a -> Museq a
append a b = let d  = _dur a + _dur b
                 va = V.map (over _1 f) $ _vec a where
                   f time = time * (_dur a / d)
                 vb = V.map (over _1 f) $ _vec b where
                   f time = time * (_dur b / d) + (_dur a / d)
             in Museq {_dur = d, _vec = (V.++) va vb}

-- | TODO : speed this up dramatically by computing start times once, rather
-- than readjusting the whole series each time a new copy is folded into it.
repeat' :: Int -> Museq a -> Museq a
repeat' k m = foldl1 append $ replicate k m

stackAsIfEqualLength :: Museq a -> Museq a -> Museq a
stackAsIfEqualLength m n =
  sortMuseq $ Museq {_dur = _dur m,
                      _vec = (V.++) (_vec m) (_vec n)}

stack :: Museq a -> Museq a -> Museq a
stack a b = let lcm = lcmRatios (_dur a) (_dur b)
                a' = repeat' (round $ lcm / _dur a) a
                b' = repeat' (round $ lcm / _dur b) b
            in stackAsIfEqualLength a' b'
