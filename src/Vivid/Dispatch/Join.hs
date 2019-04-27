{-# LANGUAGE
ScopedTypeVariables
, ViewPatterns
, FlexibleInstances #-}

module Vivid.Dispatch.Join (
    Museq(..)
  , append -- ^ forall l a. Museq l a -> Museq l a -> Museq l a
  , cat    -- ^ [Museq l a] -> Museq l a -- the name "concat" is taken
  , stack  -- ^ forall a l m. (Show l, Show m)
            -- => Museq l a  -> Museq m a -> Museq String a
  , stacks -- ^ [Museq l a] -> Museq String a
  , stack' -- ^  Museq l a  -> Museq l a -> Museq l a
  , merge  -- ^ forall a b c l m. (Show l, Show m)
            -- =>         (a ->          b ->               c)
            -- -> Museq l a -> Museq m b -> Museq String c
  , merge0, merge1, mergea -- ^ forall l m. (Show l, Show m) =>
      -- Museq l Msg -> Museq m Msg -> Museq String Msg
  , scale -- ^ forall l m. (Show l, Show m)
          -- => Museq l [Float] -> Museq m Msg -> Museq String Msg

  , meta -- ^ forall a b c l m. (Show l, Show m)
       -- => Museq l      (Museq String a -> Museq String b)
       -- -> Museq m      a
       -- -> Museq String b
  ) where

import Control.Lens (over, view, (^.))
import Data.Fixed (mod')
import qualified Data.Map as M
import qualified Data.Vector as V

import Vivid.Util
import Vivid.Dispatch.Transform
import Vivid.Dispatch.Museq
import Vivid.Dispatch.Types
import Vivid.Dispatch.Internal.Join


-- | Play one after the other
append :: forall l a. Museq l a -> Museq l a -> Museq l a
append x0 y0 = let
  durs = RTime
    $ lcmRatios (tr $ dursToPlayThrough x0) (tr $ dursToPlayThrough y0)
    -- Since x and y both have to finish at the same time,
    -- they must run through this many durs.

  xs = map adjustx ixs where
    ixs :: [(Int,V.Vector (Ev l a))]
    ixs = zip [0..] $ unsafeExplicitReps (durs * _dur x0) x0
      -- ixs uses a 0 because it starts with no ys before it
    -- adjustx: space out the xs to make room for the ys
    adjustx :: (Int,V.Vector (Ev l a)) -> V.Vector (Ev l a)
    adjustx (idx,v) = V.map f v where
      f = over evArc (\(x,y) -> (g x, g y)) where
        g = (+) $ fromIntegral idx * _dur y0

  ys = map adjusty iys where
    iys :: [(Int,V.Vector (Ev l a))]
    iys = zip [1..] $ unsafeExplicitReps (durs * _dur y0) y0
      -- iys uses a 1 because it starts with 1 (_dur x) worth of x before it
    -- adjusty: space out the ys to make room for the xs
    adjusty :: (Int,V.Vector (Ev l a)) -> V.Vector (Ev l a)
    adjusty (idx,v) = V.map f v where
      f = over evArc (\(x,y) -> (g x, g y)) where
        g = (+) $ fromIntegral idx * _dur x0

  in Museq { _sup = durs * (_dur x0 + _dur y0)
            , _dur =         _dur x0 + _dur y0
            , _vec = V.concat $ interleave xs ys }

-- todo ? speed (unlikely to matter)
-- Speed this up dramatically by computing start times once, rather
-- than readjusting the whole series each time a new copy is folded into it.
cat :: [Museq l a] -> Museq l a -- the name "concat" is taken
cat = foldl1 append

-- | Play both at the same time.
-- PITFALL: The choice of the resulting Museqs _dur is arbitrary.
-- Here it's set to that of the first.
-- For something else, just compose `Lens.set dur _` after `stack`.
stack :: forall a l m. (Show l, Show m)
       => Museq l a -> Museq m a -> Museq String a
stack x0 y0 = sortMuseq $
  _stack (labelsToStrings x0) (labelsToStrings y0)
  where
  _stack :: Museq String a -> Museq String a -> Museq String a
  _stack x y = stack' (over vec (V.map fx) x) (over vec (V.map fy) y)
    where -- append a label unused in x's events to all of y's event labels
    fx :: Ev String a -> Ev String a
    fx = over evLabel $ deleteShowQuotes
    fy :: Ev String a -> Ev String a
    fy = over evLabel $ deleteShowQuotes . (++) unusedInX
    unusedInX = unusedName $ map (view evLabel) $ V.toList $ _vec x

stacks :: [Museq String a] -> Museq String a
stacks = foldl1 _stack where
  _stack :: Museq String a -> Museq String a -> Museq String a
  _stack = stack -- only the type signature is different

-- | Allows the two arguments' namespaces to conflict
stack' :: Museq l a -> Museq l a -> Museq l a
stack' x y =
  let t = timeForBothToRepeat x y
      xs = unsafeExplicitReps t x
      ys = unsafeExplicitReps t y
  in sortMuseq $ Museq { _dur = _dur x
                         , _sup = t
                         , _vec = V.concat $ xs ++ ys}


-- | `merge`` creates a hybrid.
--
-- At any time when one of the inputs has nothing happening,
-- the output has nothing happening. Merge can increase or leave unaffected,
-- but never decrease, the amount of silence in a pattern.
--
-- When merging parameters, you'll probably usually want to use * or +,
-- on a per-parameter basis. For instance, you might want merging
-- frequencies 2 and 440 to produce a (multiplied) frequency of 880, but 
-- merging amplitudes 1 and 2 to give an (added) amplitude of 3.
--
-- `merge` is very abstract -- the two inputs can have different types.
-- For instance, it specializes to the signature
-- `Museq (a->b) -> Museq a -> Museq b` in the `Applicative Museq` instance.
--
-- PITFALL: The choice of the resulting Museqs _dur is arbitrary.
-- Here it's set to that of the second.
-- For something else, just compose `Lens.set dur _` after `stack`.

merge :: forall a b c l m. (Show l, Show m)
       =>         (a ->          b ->               c)
       -> Museq l a -> Museq m b -> Museq String c
merge op a b = _merge (labelsToStrings a) (labelsToStrings b) where
  _merge :: Museq String a
          -> Museq String b
          -> Museq String c
  _merge x y = Museq { _dur = _dur y -- arbitrary
                       , _sup = tbr
                       , _vec = V.fromList
                                 $ alignAndJoin op xps yps } where
    tbr = timeForBothToRepeat x y
    xs, xps :: [Event RTime String a]
    ys, yps :: [Event RTime String b]
    xs = concatMap V.toList $ unsafeExplicitReps tbr x
    ys = concatMap V.toList $ unsafeExplicitReps tbr y
    bs = boundaries $ map _evArc xs ++ map _evArc ys :: [RTime]
    xps = partitionAndGroupEventsAtBoundaries bs xs
    yps = partitionAndGroupEventsAtBoundaries bs ys

instance Applicative (Museq String) where -- TODO ? generalize
  (<*>) = merge ($)
  pure x = Museq { _dur=1, _sup=1
                  , _vec = V.singleton $ mkEv "" 0 1 x }


-- | Some ways to merge `Museq Msg`s.
-- So named because in math, the additive identity is 0,
-- the mutliplicative identity = 1, and "amp" starts with an "a".

merge0, merge1, mergea
  :: forall l m. (Show l, Show m) =>
  Museq l Msg -> Museq m Msg -> Museq String Msg
merge0 m n =
  merge (M.unionWith (+))  (labelsToStrings m) (labelsToStrings n)
merge1 m n =
  merge (M.unionWith (*))  (labelsToStrings m) (labelsToStrings n)
mergea m n =
  merge (M.unionWithKey f) (labelsToStrings m) $ labelsToStrings n
  where  f "amp" = (+) -- ^ add amplitudes, multiply others
         f _     = (*)

-- | Twelve tone scales (e.g. [0,2,4,5,7,9,11] = major).
scale :: forall l m. (Show l, Show m)
      => Museq l [Float] -> Museq m Msg -> Museq String Msg
scale l m = merge h (labelsToStrings l) (labelsToStrings m) where
  f :: [Float] -> Int -> Float -- lookup a pitch in a scale
  f scale n = let octave n = n `div` length scale
              in (scale !!! n) + fromIntegral (12 * octave n)

  g :: [Float] -> Float -> Float -- linear morphing between scale tones
  g scale k = let fl :: Int   = floor k
                  ce :: Int   = ceiling k
                  md :: Float = mod' k 1
              in md * f scale fl + (1-md) * f scale ce

  h :: [Float] -> Msg -> Msg
  h scale m = maybe m k $ M.lookup "freq" m where
    k :: Float -> Msg
    k freq = M.insert "freq" (g scale freq) m

meta :: forall a b l m. (Show l, Show m)
  => Museq l      (Museq String a -> Museq String b)
  -> Museq m      a
  -> Museq String b
meta x0 y0 = _meta (labelsToStrings x0) (labelsToStrings y0) where
  _meta :: Museq String (Museq String a -> Museq String b)
         -> Museq String a 
         -> Museq String b
  _meta x y = sortMuseq $ Museq { _dur = _dur y -- arbitrary
                                   , _sup = tbr
                                   , _vec = V.fromList evs } where
    tbr = timeForBothToRepeat x y
    xs :: [Ev String (Museq String a -> Museq String b)]
    xs = concatMap V.toList $ unsafeExplicitReps tbr x
    prefixLabels :: String -> Museq String a -> Museq String a
    prefixLabels s = over vec $ V.map
      $ over evLabel $ deleteShowQuotes . ((++) s)
    evs = map (over evArc $ \(s,t) -> (RTime s, RTime t))
      $ concat [arc 0 1 a b $ _evData anX $ prefixLabels (_evLabel anX) y
               | anX <- xs, let a = tr $ anX ^. evStart
                                b = tr $ anX ^. evEnd ]
