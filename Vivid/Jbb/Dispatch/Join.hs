{-# LANGUAGE ScopedTypeVariables, ViewPatterns #-}

module Vivid.Jbb.Dispatch.Join
  (
  append
  , cat
  , stack
  , merge, mergea, merge0, merge1
  , meta
  )
where

import Control.Lens (set, over, _1, _2)
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Vector as V

import Vivid.Jbb.Util
import Vivid.Jbb.Dispatch.Museq
import Vivid.Jbb.Dispatch.Types
import Vivid.Jbb.Dispatch.Internal.Join


-- | Play one after the other
append :: forall a. Museq a -> Museq a -> Museq a
append x y =
  let durs = RTime
        $ lcmRatios (tr $ dursToPlayThrough x) (tr $ dursToPlayThrough y)
        -- Since x and y both have to finish at the same time,
        -- they must run through this many durs.
      ixs, iys :: [(Int,V.Vector (Ev a))]
      ixs = zip [0..] $ unsafeExplicitReps (durs * _dur x) x
      iys = zip [1..] $ unsafeExplicitReps (durs * _dur y) y
        -- ixs uses a 0 because it starts with no ys before it
        -- iys uses a 1 because it starts with 1 (_dur x) worth of x before it

      -- next, space out the xs to make room for the ys, and vice versa
      adjustx, adjusty :: (Int,V.Vector (Ev a)) -> V.Vector (Ev a)
      adjustx (idx,v) = V.map f v where
        f = over _1 (\(x,y) -> (g x, g y)) where
          g = (+) $ fromIntegral idx * _dur y
      adjusty (idx,v) = V.map f v where
        f = over _1 (\(x,y) -> (g x, g y)) where
          g = (+) $ fromIntegral idx * _dur x
      xs, ys :: [V.Vector (Ev a)]
      xs = map adjustx ixs
      ys = map adjusty iys
  in Museq {  _sup = durs * (_dur x + _dur y)
            , _dur =         _dur x + _dur y
            , _vec = V.concat $ interleave xs ys }

-- todo ? speed this up dramatically by computing start times once, rather
-- than readjusting the whole series each time a new copy is folded into it.
cat :: [Museq a] -> Museq a -- the name "concat" is taken
cat = foldl1 append


-- | Play both at the same time.
-- PITFALL: The choice of the resulting Museq's _dur is arbitrary.
-- Here it's set to that of the first.
-- For something else, just compose `Lens.set dur _` after `stack`.
stack :: Museq a -> Museq a -> Museq a
stack x y = let t = timeForBothToRepeat x y
                xs = unsafeExplicitReps t x
                ys = unsafeExplicitReps t y
  in sortMuseq $ Museq { _dur = _dur x
                       , _sup = t
                       , _vec = V.concat $ xs ++ ys}

-- | `merge`` creates a hybrid.
--
-- At any time when one of the inputs has nothing happening,
-- the output has nothing happening.
--
-- When merging parameters, you'll probably usually want to use * or +,
-- on a per-parameter basis. For instance, you might want merging
-- frequencies 2 and 440 to produce a frequency of 880, but merging
-- amplitudes 1 and 2 to give an amplitude of 3.
--
-- `merge` is abstract. For instance, it specializes to the signature
-- `Museq (a->b) -> Museq a -> Museq b` in the `Applicative Museq` instance.
--
-- PITFALL: The choice of the resulting Museq's _dur is arbitrary.
-- Here it's set to that of the second.
-- For something else, just compose `Lens.set dur _` after `stack`.

merge :: forall a b c. (a -> b -> c) -> Museq a -> Museq b -> Museq c
merge op x y = Museq { _dur = _dur y -- arbitrary
                     , _sup = tbr
                     , _vec = V.fromList $ alignAndJoin op xps yps } where
  tbr = timeForBothToRepeat x y
  xs, xps :: [Ev a]
  ys, yps :: [Ev b]
  xs = concatMap V.toList $ unsafeExplicitReps tbr x
  ys = concatMap V.toList $ unsafeExplicitReps tbr y
  bs = boundaries $ map fst xs ++ map fst ys :: [RTime]
  xps = partitionAndGroupEventsAtBoundaries bs xs
  yps = partitionAndGroupEventsAtBoundaries bs ys

-- | Some ways to merge `Museq Msg`s.
-- So named because in math, the additive identity is 0,
-- the mutliplicative identity = 1, and "amp" starts with an "a".
mergea, merge0, merge1 :: Museq Msg -> Museq Msg -> Museq Msg
merge0 = merge $ M.unionWithKey $ const (+)
merge1 = merge $ M.unionWithKey $ const (*)
mergea = merge $ M.unionWithKey f where
  f "amp" = (+) -- ^ add amplitudes, multiply anything else
  f _ = (*)

instance Applicative Museq where
  (<*>) = merge ($)
  pure x = Museq { _dur=1, _sup=1
                 , _vec = V.singleton ((0,1),x) }

meta :: forall a b c. Museq (Museq a -> Museq b) -> Museq a -> Museq b
meta x y = sortMuseq $ Museq { _dur = _dur y -- arbitrary
                             , _sup = tbr
                             , _vec = V.fromList evs }
  where tbr = timeForBothToRepeat x y
        xs :: [Ev (Museq a -> Museq b)]
        xs = concatMap V.toList $ unsafeExplicitReps tbr x
        evs = map (over _1 $ \(s,t) -> (RTime s, RTime t))
          $ concat [arc 0 1 (tr start) (tr finish) (op y)
                   | ((start,finish),op) <- xs]

