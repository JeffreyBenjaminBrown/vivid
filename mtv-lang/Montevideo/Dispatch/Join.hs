-- | Functions to join two museqs:
-- play both simultaneously, play one after the other,
-- apply one to the other ...

{-# OPTIONS_GHC -fno-warn-orphans #-}

{-# LANGUAGE
ScopedTypeVariables
, ViewPatterns
, FlexibleInstances #-}

module Montevideo.Dispatch.Join (
    Museq(..)
  , append -- ^ forall l a. Museq l a -> Museq l a -> Museq l a
  , cat     -- ^ [Museq l a] -> Museq l a
  , stack2  -- ^ forall a l m. (Show l, Show m)
           -- => Museq l a  -> Museq m a -> Museq String a
  , stack  -- ^ [Museq l a] -> Museq String a
  , stack' -- ^  Museq l a  -> Museq l a -> Museq l a
  , merge  -- ^ forall a b c l m. (Show l, Show m)
           -- =>        (a ->         b ->              c)
           -- -> Museq l a -> Museq m b -> Museq String c
  , nMerge -- ^ forall l m. (Show l, Show m)
           -- =>        (ScMsg ->        ScMsg ->             ScMsg)
           -- -> Museq l ScMsg -> Museq m Note -> Museq String Note
  , mergec, merge0, merge1, merge0a, merge0f, merge0fa
      -- ^ forall l m. (Show l, Show m) =>
      -- Museq l ScMsg -> Museq m ScMsg -> Museq String ScMsg
  , nMergec, nMerge0, nMerge1, nMerge0a, nMerge0f, nMerge0fa
      -- ^ forall l m. (Show l, Show m) =>
      -- Museq l ScMsg -> Museq m Note -> Museq String Note
  , root -- ^ (Show l, Show m)
         -- => Museq l Float -> Museq m ScMsg -> Museq String ScMsg
  , scale     -- ^ forall l m. (Show l, Show m)
              -- => Museq l [Float] -> Museq m ScMsg -> Museq String ScMsg
  , rootScale -- ^ forall l m. (Show l, Show m)
              -- => Museq l (Float,[Float]) -> Museq m ScMsg -> Museq String ScMsg
  , meta      -- ^ forall a b c l m. (Show l, Show m)
              -- => Museq l      (Museq String a -> Museq String b)
              -- -> Museq m      a
              -- -> Museq String b
  , meta'
  , meta''
  ) where

import Control.Lens hiding (op)
import Data.Fixed (mod')
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V

import Montevideo.Dispatch.Join.Internal
import Montevideo.Dispatch.Museq
import Montevideo.Dispatch.Time
import Montevideo.Dispatch.Transform
import Montevideo.Dispatch.Types
import Montevideo.Util


instance Applicative (Museq String) where
  (<*>) = merge ($)
  pure x = Museq { _dur=1, _sup=1
                 , _vec = V.singleton $ mkEv "" 0 1 x }

instance Semigroup (Museq label a) where
  (<>) = append

instance Monoid (Museq label a) where
  mempty = Museq { _dur = 1, _sup = 1, _vec = mempty }
  mappend = append
  mconcat = cat

append :: forall l a. Museq l a -> Museq l a -> Museq l a
append x y = cat [x,y]

-- | `cat` plays its arguments one after the other.
-- It's like `Prelude.concatenate`.
--
-- The algorithm:
-- `mm` is a map from ints to those museqs
-- `f` should take a `Museq`('s index in `mm`)
--   to a list of its explicit reps,
--   spaced out to leave room for the other `Museq`s.
-- To space out rep `r` of `Museq` `i`,
-- add these to its start and end times:
--   (1) the duration of each `Museq` with index `j < i`),
--   (2) `r` * (the sum of the durations of every `Museq` except `i`)

cat :: forall l a. [Museq l a] -> Museq l a
cat ml = let
  durs = RTime $ foldr1 lcmRatios $
         map (tr . dursToFinish) ml

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
              +                  sum (map _dur earlierMuseqs)

  in Museq { _sup = durs * (sum $ map _dur $ M.elems mm)
           , _dur =         sum $ map _dur $ M.elems mm
           , _vec = V.concat $ interleaves $
                    map (f . fst) $ M.toList mm }


-- | Play both at the same time.
-- PITFALL: The choice of the resulting Museqs _dur is arbitrary.
-- Here it's set to their maximum.
-- For something else, just compose `Lens.set dur _` after `stack`.
stack2 :: forall a l m. (Show l, Show m)
       => Museq l a -> Museq m a -> Museq String a
stack2 (null . _vec -> True) x =
  x & vec %~ fmap (evLabel %~ show)
stack2 x (null . _vec -> True) =
  x & vec %~ fmap (evLabel %~ show)
stack2 x0 y0 = sortMuseq $
  _stack (labelsToStrings x0) (labelsToStrings y0)
  where
  _stack :: Museq String a -> Museq String a -> Museq String a
  _stack x y = stack' (vec %~ V.map fx $ x)
                      (vec %~ V.map fy $ y)
    where -- append a label unused in x's events to all of y's event labels
    fx :: Ev String a -> Ev String a
    fx = evLabel %~ deleteShowQuotes
    fy :: Ev String a -> Ev String a
    fy = evLabel %~ deleteShowQuotes . (unusedInX ++)
    unusedInX = unusedName $ map (view evLabel) $
      (V.toList $ _vec x) ++ (V.toList $ _vec y)

-- | Mystery: this misbehaves if `foldr1` is changed to `foldl1`.
stack :: [Museq String a] -> Museq String a
stack = foldr1 stack2

-- | Allows the two arguments' namespaces to conflict
stack' :: Museq l a -> Museq l a -> Museq l a
stack' (null . _vec -> True) x = x
stack' x (null . _vec -> True) = x
stack' x y =
  let t = timeForBoth_toFinish x y
      xs = unsafeExplicitReps t x
      ys = unsafeExplicitReps t y
  in sortMuseq $ Museq { _dur = max (_dur x) (_dur y)
                       , _sup = t
                       , _vec = V.concat $ xs ++ ys }


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
    tbr = timeForBoth_toAppearToFinish x y
    xs, xps :: [Event RTime String a]
    ys, yps :: [Event RTime String b]
    xs = concatMap V.toList $ unsafeExplicitReps tbr x
    ys = concatMap V.toList $ unsafeExplicitReps tbr y
    bs = boundaries $ map _evArc xs ++ map _evArc ys :: [RTime]
    xps = partitionAndGroupEventsAtBoundaries bs xs
    yps = partitionAndGroupEventsAtBoundaries bs ys


-- | Some ways to merge `Museq ScMsg`s.
-- So named because in math, the additive identity is 0,
-- the mutliplicative identity = 1, and "amp" starts with an "a".
-- The 'n' prefix indicates that the second arg is a Note, not a ScMsg.

nMerge  :: forall l m. (Show l, Show m)
  => (ScMsg -> ScMsg -> ScMsg)
  -> Museq l ScMsg -> Museq m Note -> Museq String Note
nMerge op = merge f
  where f :: ScMsg -> Note -> Note
        f m1 (Note synth m1') = Note synth $ op m1 m1'

mergec, merge0, merge1, merge0a, merge0f, merge0fa
  :: forall l m. (Show l, Show m) =>
  Museq l ScMsg -> Museq m ScMsg -> Museq String ScMsg
nMergec, nMerge0, nMerge1, nMerge0a, nMerge0f, nMerge0fa
  :: forall l m. (Show l, Show m) =>
  Museq l ScMsg -> Museq m Note -> Museq String Note

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
     => Museq l Float -> Museq m ScMsg -> Museq String ScMsg
root mr = meta $ f <$> mr where
  f n = overParams [("freq", (+) n)]

-- | Twelve tone scales (e.g. [0,2,4,5,7,9,11] = major).
scale :: forall l m. (Show l, Show m)
      => Museq l [Float] -> Museq m ScMsg -> Museq String ScMsg
scale l0 m0 = merge h (labelsToStrings l0) (labelsToStrings m0) where
  f :: [Float] -> Int -> Float -- lookup a pitch in a scale
  f scale0 n0 = let octave n = n `div` length scale0
                in (scale0 !!! n0) + fromIntegral (12 * octave n0)

  g :: [Float] -> Float -> Float -- linear morphing between scale tones
  g scale0 k = let fl :: Int   = floor k
                   ce :: Int   = ceiling k
                   md :: Float = mod' k 1
              in md * f scale0 fl + (1-md) * f scale0 ce

  h :: [Float] -> ScMsg -> ScMsg
  h scale0 m = maybe m k $ M.lookup "freq" m where
    k :: Float -> ScMsg
    k freq = M.insert "freq" (g scale0 freq) m

rootScale :: forall l m. (Show l, Show m)
      => Museq l (Float,[Float]) -> Museq m ScMsg -> Museq String ScMsg
rootScale mrs = let roots  = fst <$> mrs
                    scales = snd <$> mrs
  in root roots . scale scales

meta :: forall l m x y. (Show l, Show m)
  => Museq l      (Museq String x -> Museq String y)
  -> Museq m      x
  -> Museq String y
meta f0 x0 = _meta (labelsToStrings f0) (labelsToStrings x0) where
  _meta :: Museq String (Museq String x -> Museq String y)
        -> Museq String x
        -> Museq String y

  _meta f x = sortMuseq $ Museq { _dur = _dur x -- arbitrary
                                , _sup = tbr
                                , _vec = V.fromList evs } where
    tbr = timeForBoth_toAppearToFinish f x
    fs :: [Ev String (Museq String x -> Museq String y)]
    fs = concatMap V.toList $ unsafeExplicitReps tbr f
    prefixLabels :: String -> Museq String x -> Museq String x
    prefixLabels s = over vec $ V.map
      $ over evLabel $ deleteShowQuotes . ((++) s)
    evs :: [Ev String y]
    evs = map (evArc . both %~ RTime)
      $ concat [ arc 0 1 a b $ _evData anF $
                 prefixLabels (_evLabel anF) x
               | anF <- fs, let a = tr $ anF ^. evStart
                                b = tr $ anF ^. evEnd ]

meta' :: forall l m x y. (Show l, Show m)
  => Museq l      (Museq String x -> Museq String y)
  -> Museq m      x
  -> [Ev String y]
meta' f0 x0 = _meta (labelsToStrings f0) (labelsToStrings x0) where
  _meta :: Museq String (Museq String x -> Museq String y)
        -> Museq String x
        -> [Ev String y]

  _meta f x = evs where
    tbr = timeForBoth_toAppearToFinish f x
    fs :: [Ev String (Museq String x -> Museq String y)]
    fs = concatMap V.toList $ unsafeExplicitReps tbr f
    prefixLabels :: String -> Museq String x -> Museq String x
    prefixLabels s = over vec $ V.map
      $ over evLabel $ deleteShowQuotes . ((++) s)
    evs :: [Ev String y]
    evs = map (evArc . both %~ RTime)
      $ concat [ arc 0 1 a b $ _evData anF $
                 prefixLabels (_evLabel anF) x
               | anF <- fs, let a = tr $ anF ^. evStart
                                b = tr $ anF ^. evEnd ]

-- | Unfinished. The idea is to let me see what's going on in `meta`,
-- by pairing each function in the first argument with a string.
-- To be used in conjunction with "bugs/empty-stream.hs".

meta'' :: forall l m x y. (Show l, Show m)
  => Museq l      ( String
                  , Museq String x -> Museq String y)
  -> Museq m      x
  -> ( RTime
     , [( Ev String String, Museq String x )]
     , [Event Time String y] )
meta'' ff0 x0 = _meta (labelsToStrings ff0) (labelsToStrings x0) where

  _meta :: Museq String ( String
                        , Museq String x -> Museq String y)
        -> Museq String x
        -> ( RTime
           , [( Ev String String, Museq String x )]
           , [Event Time String y] )

  _meta ff1 x1 = (tbr, fxs, ews) where
    tbr = timeForBoth_toAppearToFinish ff1 x1

    ffs :: [Ev String ( String
                      , Museq String x -> Museq String y )]
    ffs = concatMap V.toList $ unsafeExplicitReps tbr ff1
    prefixLabels :: String -> Museq String x -> Museq String x
    prefixLabels s = vec %~ V.map
                     (evLabel %~ deleteShowQuotes . (s ++))

    -- See here for why I'm commenting out this type signature.
      -- https://github.com/haskell/haskell-mode/issues/1652
    -- ffxs :: [( Ev String (String, Museq String x -> Museq String y)
    --          , Museq String x )]
    ffxs = [ ( anFf, prefixLabels (_evLabel anFf) x1)
           | anFf <- ffs ]
    fxs :: [( Ev String String, Museq String x )]
    fxs = map (_1 %~ fmap fst) ffxs

    ews :: [Event Time String y]
    ews = concatMap g ffxs where
      g (ff,x2) = let a = tr $ ff ^. evStart
                      b = tr $ ff ^. evEnd
                      f = snd $ _evData ff
        in arc 0 1 a b $ f x2
