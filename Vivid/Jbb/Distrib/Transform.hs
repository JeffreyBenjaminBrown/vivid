{-# LANGUAGE ScopedTypeVariables #-}

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
cat :: [Museq a] -> Museq a
cat = foldl1 append

-- | TODO : this ought to accept positive nonintegers
repeat' :: Int -> Museq a -> Museq a
repeat' k = cat . replicate k

stackAsIfEqualLength :: Museq a -> Museq a -> Museq a
stackAsIfEqualLength m n =
  sortMuseq $ Museq {_dur = _dur m,
                      _vec = (V.++) (_vec m) (_vec n)}

stack :: Museq a -> Museq a -> Museq a
stack a b = let lcm = lcmRatios (_dur a) (_dur b)
                a' = repeat' (round $ lcm / _dur a) a
                b' = repeat' (round $ lcm / _dur b) b
            in stackAsIfEqualLength a' b'

-- todo ? sorting in `rev` is overkill; faster would be to move the
-- elements at time=1, if they exist, to time=0
rev :: Museq a -> Museq a
rev = sortMuseq . over vec g
  where g = V.reverse . V.map (over _1 f)
        f x = if 1-x < 1 then 1-x else 0

-- todo ? sorting in `early` or `late` is overkill too
early :: RDuration -> Museq a -> Museq a
early t m = sortMuseq $ over vec (V.map $ over _1 f) m
  where t' = let pp0 = prevPhase0 0 (_dur m) t
             in t - pp0
        f s = let s' = s - t' / _dur m
              in if s' < 0 then s'+1 else s'

late :: RDuration -> Museq a -> Museq a
late t m = sortMuseq $ over vec (V.map $ over _1 f) m
  where t' = let pp0 = prevPhase0 0 (_dur m) t
             in t - pp0
        f s = let s' = s + t' / _dur m
              in if s' >= 1 then s'-1 else s'

fast :: RDuration -> Museq a -> Museq a
fast d = over dur (/d)

slow :: RDuration -> Museq a -> Museq a
slow d = over dur (*d)

dense :: forall a. RDuration -> Museq a -> Museq a
dense d m = let cd = ceiling d :: Int
                indexedMs = zip [0..cd-1] $ repeat m :: [(Int,Museq a)]
                shiftedMs :: [Museq a]
                shiftedMs = map (\(idx,msq) ->
                                   over vec (V.map $ over _1
                                            $ (/d) . (+ fromIntegral idx))
                                   msq)
                            indexedMs
                in Museq { _dur = _dur m
                         , _vec = V.filter ((< 1) . fst)
                                  $ V.concat $ map _vec shiftedMs}
