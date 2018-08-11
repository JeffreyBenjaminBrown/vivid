{-# LANGUAGE ScopedTypeVariables, ViewPatterns #-}

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
             in Museq {_dur = d, _sup = d, _vec = (V.++) va vb}

-- | example:
-- > x
-- [(1,"gobot"),(2,"gobot"),(3,"gobot"),(4,"gobot"),(5,"gobot"),(6,"gobot")]
-- > divideAtMaxima fst [3,5] $ V.fromList x
-- [[(1,"gobot"),(2,"gobot")]
-- ,[(3,"gobot"),(4,"gobot")]]
divideAtMaxima :: forall a b. Ord b
               => (a->b) -> [b] -> V.Vector a -> [V.Vector a]
divideAtMaxima view tops stuff = reverse $ go [] tops stuff where
  go :: [V.Vector a] -> [b] -> V.Vector a -> [V.Vector a]
  go acc _     (V.null -> True) = acc
  go acc []     _               = acc
  go acc (t:ts) vec             =
    let (lt,gte) = V.partition ((< t) . view) vec
    in go (lt : acc) ts gte

-- | if L is the length of time such that `m` finishes at phase 0,
-- divide the events of L every multiple of _dur.
-- See the test suite for an example.

explicitReps :: forall a. Museq a -> [V.Vector (RTime,a)]
explicitReps m = unsafeExplicitReps maxTime m
  where maxTime = lcmRatios (_sup m) (_dur m)

-- | PITFALL: I don't know what this will do if
-- `maxTime` is not an integer multiple of `lcmRatios (_sup m) (_dur m)`
unsafeExplicitReps :: forall a.
  RTime -> Museq a -> [V.Vector (RTime,a)]
unsafeExplicitReps maxTime m =
  let sups = round $ maxTime / (_sup m)
        -- It takes a duration equal to this many multiples of _sup m
        -- for m to finish at phase 0.
        -- It's already an integer; round is just to prove that to GHC.
      durs = round $ maxTime / (_dur m)
      indexed = zip [0..sups-1]
        $ repeat $ _vec m :: [(Int,V.Vector (RTime,a))]
      adjustTimes :: (Int,V.Vector (RTime,a))
                  ->      V.Vector (RTime,a)
      adjustTimes (idx,v) = V.map f v where
        f = over _1 $ (+) (fromIntegral idx * _sup m)
      spread = map adjustTimes indexed :: [V.Vector (RTime,a)]
        -- the times in spread range from 0 to lcmRatios (_sup m) (_dur m)
      concatted = V.concat spread :: V.Vector (RTime,a)
      reps = divideAtMaxima fst [fromIntegral i * _dur m
                                | i <- [1..durs]] concatted
        :: [V.Vector (RTime,a)]
  in reps

-- | the `sup`-aware append
--append' :: Museq a -> Museq a -> Museq a
--append' a b = let toFinish m = lcmRatios (_dur m) (_sup m) / _sup m
--  -- `toFinish` is the number of its `sup`s a Museq must play through
--  -- in order to finish at phase 0. For instance, dur = 2 and sup = 3
--  -- imply it will be ready to start all over at (relative) time 6.
--                 total = lcmRatios (toFinish a) (toFinish b)
--                 va = V.map (over _1 f) $ _vec a where
--                   f time = time * (_dur a / d)
--                 vb = V.map (over _1 f) $ _vec b where
--                   f time = time * (_dur b / d) + (_dur a / d)
--             in Museq {_dur = d, _sup = d, _vec = (V.++) va vb}

-- | TODO : speed this up dramatically by computing start times once, rather
-- than readjusting the whole series each time a new copy is folded into it.
-- It would look something like the dense algorithm, but without the ceiling,
-- and with each Museq's RTimes being adjusted as a function of that Museq's
-- duration, the duration of the whole collection, and the duration of those
-- Museq's prior to it. Data.List.scanl should help.
cat :: [Museq a] -> Museq a
cat = foldl1 append

-- | TODO : this ought to accept positive nonintegers
repeat' :: Int -> Museq a -> Museq a
repeat' k = cat . replicate k

stackAsIfEqualLength :: Museq a -> Museq a -> Museq a
stackAsIfEqualLength m n =
  sortMuseq $ Museq { _dur = _dur m, _sup = _sup m
                    , _vec = (V.++) (_vec m) (_vec n)}

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
                         , _sup = _sup m
                         , _vec = V.filter ((< 1) . fst)
                                  $ V.concat $ map _vec shiftedMs}
