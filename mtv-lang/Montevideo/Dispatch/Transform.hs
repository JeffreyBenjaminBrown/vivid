{-# LANGUAGE ScopedTypeVariables, ViewPatterns #-}

module Montevideo.Dispatch.Transform (
    rev                    -- ^               Museq l a -> Museq l a
  , early, late            -- ^ RDuration  -> Museq l a -> Museq l a
  , fast,slow,dense,sparse -- ^ Rational   -> Museq l a -> Museq l a
  , rotate, rep            -- ^ Rational   -> Museq l a -> Museq l a

  , overParams   -- ^ [(ParamName, Float -> Float)] -> Museq l Msg -> Museq l Msg
  , switchParams -- ^ [(ParamName, ParamName)]      -> Museq l Msg -> Museq l Msg
  , keepParams   -- ^ [ParamName]                   -> Museq l Msg -> Museq l Msg
  , dropParams   -- ^ [ParamName]                   -> Museq l Msg -> Museq l Msg
  ) where

import Control.Lens
import Data.Fixed (mod')
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V

import Montevideo.Dispatch.Types


-- | Reverse a `Museq`
rev :: Museq l a -> Museq l a -- the name "reverse" is taken
rev m = vec %~ sortIt . g $ m where
  -- todo ? sorting in `rev` is overkill; faster would be to move the
  -- elements at time=1, if they exist, to time=0
  g :: V.Vector (Ev l a) -> V.Vector (Ev l a)
  g = V.reverse . V.map (over evArc f) where
    s = _sup m
    f (x,y) = if x > 0
              then (s-x, (s-x) + (y-x))
              else (x,y)
  sortIt :: V.Vector (Ev l a) -> V.Vector (Ev l a)
  sortIt v = let (a,b) = V.partition ((==) 0 . fst . _evArc) v
             in a V.++ b

-- todo ? sorting in `early` or `late` is overkill, similar to `rev`
early, late :: RDuration -> Museq l a -> Museq l a
early t m =
  vec %~ (sortIt . V.map (evArc %~ shift)) $ m
  where
  shift :: (RTime,RTime) -> (RTime,RTime)
  shift (x,y) = (x',x' + (y-x))
    where
    x' = shiftStart x
      where
      shiftStart :: RTime -> RTime
      shiftStart rt = mod' (rt - t) (_sup m)
  sortIt v = let (a,b) = V.partition ((>=) t . fst . _evArc ) v
             in a V.++ b
late t = early (-t)

-- | `dense` and `sparse` are like `fast` and `slow`, but whereas
-- making something e.g. twice as fast shrinks its sup and its dur,
-- making it twice as dense shrinks only is sup.
-- For a single music, they are indistinguishable.
-- But when concatenated alongside other patterns,
-- `fast` and `slow` will change the duration of the pattern,
-- while `dense` and `sparse` will not.
fast,slow,dense,sparse :: Rational -> Museq l a -> Museq l a
fast d m = let f = (/ (RTime d))
               g (x,y) = (f x, f y)
  in over dur f $ over sup f $ over vec (V.map $ over evArc g) $ m
slow d m = let f = (* (RTime d))
               g (x,y) = (f x, f y)
  in over dur f $ over sup f $ over vec (V.map $ over evArc g) $ m
dense d m = let f = (/ (RTime d))
                g (x,y) = (f x, f y)
  in              over sup f $ over vec (V.map $ over evArc g) $ m
sparse d m = let f = (* (RTime d))
                 g (x,y) = (f x, f y)
  in              over sup f $ over vec (V.map $ over evArc g) $ m


rotate, rep :: Rational -> Museq l a -> Museq l a

-- | If `m` has sup and dur 1 s, then `rotate 2 m`
-- has the same sup but half the dur.
-- Thus the first time it plays, it will play the first half,
-- and the next time it plays, it will play the second half.
-- If not concatenated against another pattern, then for all k,
-- `m` and `rotate k m` sound identical in events and timing.
--
-- Note that `rep k == rotate (1/k)`
rotate t = fast t . sparse t

-- | If `m` has sup and dur 1 s, then `rep 2 m` has duration 2 s,
-- and is otherwise the same as `m`.
-- As the only thing playing, they sound identical in events and timing.
-- But they become different when concatenated with other patterns --
-- the `m` will only play for 1 second before yielding to the next,
-- whereas `rep 2 m` will play for 2 seconds.
--
-- Note that `rep k == rotate (1/k)`
rep n = slow n . dense n


-- | = (something) -> Museq Msg -> Museq Msg
overParams :: [(ParamName, Float -> Float)] -> Museq l Msg -> Museq l Msg
overParams fs = fmap $ M.mapWithKey g
  -- TODO ? speed: This appears to look up each of each Msg's parameters.
  -- If the Msg is smaller than the argument to overParams, that'll work.
  -- But if it's bigger, that's inefficient.
  where g :: ParamName -> Float -> Float
        -- For each parameter found, apply the function to it;
        -- if not found, don't apply anything to it.
        g k v = maybe v ($v) $ M.lookup k $ M.fromList fs

-- | `switchParams [(a,b)]` will replace each instance of `a`
-- as a parameter name in every input `Msg` to `b`.
switchParams :: [(ParamName, ParamName)] -> Museq l Msg -> Museq l Msg
switchParams fs = fmap $ M.mapKeys g where
  -- TODO ? speed: This appears to look up each of each Msg's parameters.
  -- If the Msg is smaller than the argument to overParams, that'll work.
  -- But if it's bigger, that's inefficient.
  g :: ParamName -> ParamName
  g k = maybe k id $ M.lookup k $ M.fromList fs

keepParams :: [ParamName] -> Museq l Msg -> Museq l Msg
keepParams ps = over vec $ V.filter (not . null . view evData)
                 . (V.map $ over evData $ flip M.restrictKeys $ S.fromList ps)

dropParams :: [ParamName] -> Museq l Msg -> Museq l Msg
dropParams ps = over vec $ V.filter (not . null . view evData)
                 . (V.map $ over evData $ flip M.withoutKeys $ S.fromList ps)
