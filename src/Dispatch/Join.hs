{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE
ScopedTypeVariables
, ViewPatterns
, FlexibleInstances #-}

module Dispatch.Join (
    Museq(..)
  , append -- ^ forall l a. Museq l a -> Museq l a -> Museq l a
  , appends -- ^ [Museq l a] -> Museq l a
  , cat     -- ^ [Museq l a] -> Museq l a -- the name "concat" is taken
  , stack  -- ^ forall a l m. (Show l, Show m)
           -- => Museq l a  -> Museq m a -> Museq String a
  , stacks -- ^ [Museq l a] -> Museq String a
  , stack' -- ^  Museq l a  -> Museq l a -> Museq l a
  , merge  -- ^ forall a b c l m. (Show l, Show m)
           -- =>        (a ->         b ->              c)
           -- -> Museq l a -> Museq m b -> Museq String c
  , nMerge -- ^ forall l m. (Show l, Show m)
           -- =>        (Msg ->         Msg ->               Msg)
           -- -> Museq l Msg -> Museq m Note -> Museq String Note
  , mergec, merge0, merge1, merge0a, merge0f, merge0fa
      -- ^ forall l m. (Show l, Show m) =>
      -- Museq l Msg -> Museq m Msg -> Museq String Msg
  , nMergec, nMerge0, nMerge1, nMerge0a, nMerge0f, nMerge0fa
      -- ^ forall l m. (Show l, Show m) =>
      -- Museq l Msg -> Museq m Note -> Museq String Note
  , root -- ^ (Show l, Show m)
         -- => Museq l Float -> Museq m Msg -> Museq String Msg
  , scale -- ^ forall l m. (Show l, Show m)
          -- => Museq l [Float] -> Museq m Msg -> Museq String Msg
  , rootScale -- ^ forall l m. (Show l, Show m)
             -- => Museq l (Float,[Float]) -> Museq m Msg -> Museq String Msg
  , meta -- ^ forall a b c l m. (Show l, Show m)
       -- => Museq l      (Museq String a -> Museq String b)
       -- -> Museq m      a
       -- -> Museq String b
  ) where

import Control.Lens hiding (op)
import Data.Fixed (mod')
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V

import Util
import Dispatch.Museq
import Dispatch.Types
import Dispatch.Internal.Join
import Dispatch.Transform


-- | Play one after the other
append :: forall l a. Museq l a -> Museq l a -> Museq l a
append x0 y0 = let
  durs = RTime $ lcmRatios
         (tr $ dursToPlayThrough x0)
         (tr $ dursToPlayThrough y0)
    -- Since x and y both have to finish at the same time,
    -- they must run through this many durs.

  xs = map adjustx ixs where
    ixs :: [(Int,V.Vector (Ev l a))]
      -- ixs uses a 0 because it starts with no ys before it
    ixs = zip [0..] $ unsafeExplicitReps (durs * _dur x0) x0

    adjustx :: (Int,V.Vector (Ev l a)) -> V.Vector (Ev l a)
      -- adjustx: space out the xs to make room for the ys
    adjustx (idx,v) = V.map f v where
      f = evArc . both %~ g where
        g = (+) $ fromIntegral idx * _dur y0

  ys = map adjusty iys where
    iys :: [(Int,V.Vector (Ev l a))]
      -- iys uses a 1 because it starts with 1 (_dur x) worth of x before it
    iys = zip [1..] $ unsafeExplicitReps (durs * _dur y0) y0
    adjusty :: (Int,V.Vector (Ev l a)) -> V.Vector (Ev l a)
      -- adjusty: space out the ys to make room for the xs
    adjusty (idx,v) = V.map f v where
      f = evArc . both %~ g where
        g = (+) $ fromIntegral idx * _dur x0

  in Museq { _sup = durs * (_dur x0 + _dur y0)
           , _dur =         _dur x0 + _dur y0
           , _vec = V.concat $ interleave xs ys }


-- | The `append` algorithm:
-- `ml` is a list of museqs
-- `mm` is a map from ints to those museqs
-- `f` should take a `Museq`('s index) to
--   a list of its explicit reps,
--   spaced out to leave room for the other museqs
-- To space out rep `r` of `Museq` `i`
--   add `i` * (the duration of each `Museq` with index `j < i`)
--   add `r` * (the sum of the durations of every `Museq` except `i`)

appends' :: forall l a. [Museq l a] -> Museq l a
appends' ml = let
  durs = RTime $ foldr1 lcmRatios $
         map (tr . dursToPlayThrough) ml

  mm :: M.Map Int (Museq l a) =
    M.fromList $ zip [0..] ml

  f :: Int -> [V.Vector (Ev l a)]
    -- Each `n` is the key bound to a `Museq` `m`.
  f n = map adjust iReps where
    m :: Museq l a = mm M.! n

    iReps :: [(Int,V.Vector (Ev l a))]
    iReps = zip [0..] $
      unsafeExplicitReps (durs * _dur m) m

    adjust :: (Int,V.Vector (Ev l a)) -> V.Vector (Ev l a)
      -- Each `r` is a repetition of `m`.
    adjust (r,v) = V.map (evArc . both %~ g) v where
      otherMuseqs   = M.elems $ M.delete n mm -- everything but n
      earlierMuseqs = M.elems $ M.restrictKeys mm $
                      S.filter (< n) $ -- everything before n
                      M.keysSet mm
      g = (+) $ fromIntegral r * sum (map _dur otherMuseqs)
              + fromIntegral n * sum (map _dur earlierMuseqs)

  in Museq { _sup = durs * (sum $ map _dur $ M.elems mm)
           , _dur =         sum $ map _dur $ M.elems mm
           , _vec = V.concat $ interleaves $
                    map (f . fst) $ M.toList mm }


-- TODO : if `appends` works, this is obsolete

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
    unusedInX = unusedName $ map (view evLabel) $
      (V.toList $ _vec x) ++ (V.toList $ _vec y) 

stacks :: [Museq String a] -> Museq String a
stacks = foldl1 stack

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
-- The 'n' prefix indicates that the second arg is a Note, not a Msg.

nMerge  :: forall l m. (Show l, Show m)
  => (Msg -> Msg -> Msg)
  -> Museq l Msg -> Museq m Note -> Museq String Note
nMerge op = merge f
  where f :: Msg -> Note -> Note
        f m1 (Note synth m1') = Note synth $ op m1 m1'

mergec, merge0, merge1, merge0a, merge0f, merge0fa
  :: forall l m. (Show l, Show m) =>
  Museq l Msg -> Museq m Msg -> Museq String Msg
nMergec, nMerge0, nMerge1, nMerge0a, nMerge0f, nMerge0fa
  :: forall l m. (Show l, Show m) =>
  Museq l Msg -> Museq m Note -> Museq String Note

mergec m n =
  merge (M.unionWith const)  (labelsToStrings m) (labelsToStrings n)
nMergec m n =
  nMerge (M.unionWith const)  (labelsToStrings m) (labelsToStrings n)

merge0 m n =
  merge (M.unionWith (+))  (labelsToStrings m) (labelsToStrings n)
nMerge0 m n =
  nMerge (M.unionWith (+))  (labelsToStrings m) (labelsToStrings n)

nMerge1 m n =
  nMerge (M.unionWith (*))  (labelsToStrings m) (labelsToStrings n)
merge1 m n =
  merge (M.unionWith (*))  (labelsToStrings m) (labelsToStrings n)

nMerge0a m n =
  nMerge (M.unionWithKey f) (labelsToStrings m) $ labelsToStrings n
  where f "amp" = (+) -- ^ add amplitudes, multiply others
        f _     = (*)
merge0a m n =
  merge (M.unionWithKey f) (labelsToStrings m) $ labelsToStrings n
  where f "amp" = (+) -- ^ add amplitudes, multiply others
        f _     = (*)

merge0f m n =
  merge (M.unionWithKey f) (labelsToStrings m) $ labelsToStrings n
  where f "freq" = (+) -- ^ add frequencies, multiply others
        f _      = (*)
nMerge0f m n =
  nMerge (M.unionWithKey f) (labelsToStrings m) $ labelsToStrings n
  where f "freq" = (+) -- ^ add frequencies, multiply others
        f _      = (*)

merge0fa m n =
  merge (M.unionWithKey f) (labelsToStrings m) $ labelsToStrings n
  where f "freq" = (+) -- ^ add frequencies
        f "amp" = (+)  -- ^ add amplitudes
        f _      = (*) -- ^ multiply others
nMerge0fa m n =
  nMerge (M.unionWithKey f) (labelsToStrings m) $ labelsToStrings n
  where f "freq" = (+) -- ^ add frequencies
        f "amp" = (+)  -- ^ add amplitudes
        f _      = (*) -- ^ multiply others


root :: (Show l, Show m)
     => Museq l Float -> Museq m Msg -> Museq String Msg
root mr = meta $ f <$> mr where
  f n = overParams [("freq", (+) n)]

-- | Twelve tone scales (e.g. [0,2,4,5,7,9,11] = major).
scale :: forall l m. (Show l, Show m)
      => Museq l [Float] -> Museq m Msg -> Museq String Msg
scale l0 m0 = merge h (labelsToStrings l0) (labelsToStrings m0) where
  f :: [Float] -> Int -> Float -- lookup a pitch in a scale
  f scale0 n0 = let octave n = n `div` length scale0
                in (scale0 !!! n0) + fromIntegral (12 * octave n0)

  g :: [Float] -> Float -> Float -- linear morphing between scale tones
  g scale0 k = let fl :: Int   = floor k
                   ce :: Int   = ceiling k
                   md :: Float = mod' k 1
              in md * f scale0 fl + (1-md) * f scale0 ce

  h :: [Float] -> Msg -> Msg
  h scale0 m = maybe m k $ M.lookup "freq" m where
    k :: Float -> Msg
    k freq = M.insert "freq" (g scale0 freq) m

rootScale :: forall l m. (Show l, Show m)
      => Museq l (Float,[Float]) -> Museq m Msg -> Museq String Msg
rootScale mrs = let roots  = fst <$> mrs
                    scales = snd <$> mrs
  in root roots . scale scales

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
