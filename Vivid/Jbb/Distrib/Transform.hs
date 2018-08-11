{-# LANGUAGE ScopedTypeVariables, ViewPatterns #-}

module Vivid.Jbb.Distrib.Transform where

import Control.Lens (over, _1)
import qualified Data.Vector as V

import Vivid.Jbb.Util
import Vivid.Jbb.Distrib.Museq
import Vivid.Jbb.Distrib.Types


-- | if L is the length of time such that `m` finishes at phase 0,
-- divide the events of L every multiple of _dur.
-- See the test suite for an example.
explicitReps :: forall a. Museq a -> [V.Vector (RTime,a)]
explicitReps m = unsafeExplicitReps (timeToPlayThrough m) m

-- | PITFALL: I don't know what this will do if
-- `totalDuration` is not an integer multiple of `timeToPlayThrough m`
unsafeExplicitReps :: forall a.
  RTime -> Museq a -> [V.Vector (RTime,a)]
unsafeExplicitReps totalDuration m =
  let sups = round $ totalDuration / (_sup m)
        -- It takes a duration equal to this many multiples of _sup m
        -- for m to finish at phase 0.
        -- It's already an integer; `round` is just to prove that to GHC.
      durs = round $ totalDuration / (_dur m)
      indexed = zip [0..sups-1]
        $ repeat $ _vec m :: [(Int,V.Vector (RTime,a))]
      adjustTimes :: (Int,V.Vector (RTime,a))
                  ->      V.Vector (RTime,a)
      adjustTimes (idx,v) = V.map f v where
        f = over _1 $ (+) (fromIntegral idx * _sup m)
      spread = map adjustTimes indexed :: [V.Vector (RTime,a)]
        -- the times in `spread` range from 0 to `timeToRepat m`
      concatted = V.concat spread :: V.Vector (RTime,a)
      reps = divideAtMaxima fst [fromIntegral i * _dur m
                                | i <- [1..durs]] concatted
        :: [V.Vector (RTime,a)]
  in reps

-- | the `sup`-aware append
append :: forall a. Museq a -> Museq a -> Museq a
append x y =
  let durs = lcmRatios (dursToPlayThrough x) (dursToPlayThrough y)
        -- Since x and y both have to finish at the same time,
        -- they must run through this many durs.
      ixs, iys :: [(Int,V.Vector (RTime,a))]
      ixs = zip [0..] $ unsafeExplicitReps (durs * _dur x) x
      iys = zip [1..] $ unsafeExplicitReps (durs * _dur y) y
        -- ixs uses a 0 because it starts with no ys before it
        -- iys uses a 1 because it starts with 1 (_dur x) worth of x before it

      -- next, space out the xs to make room for the ys, and vice versa
      adjustx :: (Int,V.Vector (RTime,a))
              ->      V.Vector (RTime,a)
      adjustx (idx,v) = V.map f v where
        f = over _1 $ (+) (fromIntegral idx * _dur y)
      adjusty :: (Int,V.Vector (RTime,a))
              ->      V.Vector (RTime,a)
      adjusty (idx,v) = V.map f v where
        f = over _1 $ (+) (fromIntegral idx * _dur x)
      xs, ys :: [V.Vector (RTime,a)]
      xs = map adjustx ixs
      ys = map adjusty iys
  in Museq { _sup = durs * (_dur x + _dur y)
           , _dur = _dur x + _dur y
           , _vec = V.concat $ interleave xs ys }

-- | todo : speed this up dramatically by computing start times once, rather
-- than readjusting the whole series each time a new copy is folded into it.
cat :: [Museq a] -> Museq a
cat = foldl1 append

-- | todo : this ought to accept positive nonintegers
repeat' :: Int -> Museq a -> Museq a
repeat' k = cat . replicate k

-- | Play both at the same time.
-- PITFALL: The choice of the resulting Museq's _dur is arbitrary.
-- Here it's that of the first; for something else just `Lens.set dur`
stack :: Museq a -> Museq a -> Museq a
stack x y = let tx = timeToRepeat x
                ty = timeToRepeat y
                t = lcmRatios tx ty
                xs = unsafeExplicitReps t x
                ys = unsafeExplicitReps t y
  in sortMuseq $ Museq {_dur = _dur x, _sup = t, _vec = V.concat $ xs ++ ys}

-- todo ? sorting in `rev` is overkill; faster would be to move the
-- elements at time=1, if they exist, to time=0
rev :: Museq a -> Museq a
rev m = sortMuseq $ over vec g m
  where s = _sup m
        g = V.reverse . V.map (over _1 f)
        f x = if s-x < s then s-x else 0

-- todo ? sorting in `early` or `late` is overkill, similar to `rev`
early :: RDuration -> Museq a -> Museq a
early t m = sortMuseq $ over vec (V.map $ over _1 f) m
  where t' = let pp0 = prevPhase0 0 (_dur m) t
             in t - pp0
        f s = let s' = s - t'
              in if s' < 0 then s'+_sup m else s'

late :: RDuration -> Museq a -> Museq a
late t m = sortMuseq $ over vec (V.map $ over _1 f) m
  where t' = let pp0 = prevPhase0 0 (_dur m) t
             in t - pp0
        f s = let s' = s + t'
              in if s' >= _sup m then s'-_sup m else s'

fast :: Rational -> Museq a -> Museq a
fast d m = let f = (/d)
  in over dur f $ over sup f $ over vec (V.map $ over _1 f) $ m

slow :: Rational -> Museq a -> Museq a
slow d m = let f = (*d)
  in over dur f $ over sup f $ over vec (V.map $ over _1 f) $ m

dense :: Rational -> Museq a -> Museq a
dense d m = let f = (/d)
  in              over sup f $ over vec (V.map $ over _1 f) $ m

sparse :: Rational -> Museq a -> Museq a
sparse d m = let f = (*d)
  in              over sup f $ over vec (V.map $ over _1 f) $ m
