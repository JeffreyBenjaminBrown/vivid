{-# LANGUAGE ScopedTypeVariables, ViewPatterns #-}

module Vivid.Jbb.Dispatch.Transform (
  rev
  , rev'
  , early, late
  , early', late'
  , fast, slow
  , fast', slow'
  , dense, sparse
  , dense', sparse'
  , rotate, rep

  , overParams
  , switchParams
  , keepParams
  , dropParams

  , overParams'
  , switchParams'
  , keepParams'
  , dropParams'
  ) where

import Control.Lens (over, _1, _2)
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V

import Vivid.Jbb.Util
import Vivid.Jbb.Dispatch.Museq
import Vivid.Jbb.Dispatch.Types


-- todo ? sorting in `rev` is overkill; faster would be to move the
-- elements at time=1, if they exist, to time=0
rev :: Museq a -> Museq a -- the name "reverse" is taken
rev m = sortMuseq $ over vec g m
  where s = _sup m
        g = V.reverse . V.map (over _1 f)
        f x = if x > 0 then s-x else 0

rev' :: Museq' a -> Museq' a -- the name "reverse" is taken
rev' m = sortMuseq' $ over vec' g m where
  g = V.reverse . V.map (over _1 f) where
    s = _sup' m
    f (x,y) = if x > 0
              then (s-x, (s-x) + (y-x))
              else (x,y)

-- todo ? sorting in `early` or `late` is overkill, similar to `rev`
early, late :: RDuration -> Museq a -> Museq a
early t m = sortMuseq $ over vec (V.map $ over _1 f) m
  where t' = let pp0 = prevPhase0 0 (_dur m) t
             in t - pp0
        f s = let s' = s - t'
              in if s' < 0 then s'+_sup m else s'
late t m = sortMuseq $ over vec (V.map $ over _1 f) m
  where t' = let pp0 = prevPhase0 0 (_dur m) t
             in t - pp0
        f s = let s' = s + t'
              in if s' >= _sup m then s'-_sup m else s'

-- TODO : early, late don't handle negative numbers correctly
early' :: RDuration -> Museq' a -> Museq' a
early' t m = sortMuseq' $ over vec' (V.map $ over _1 f) m
  where t' = let pp0 = prevPhase0 0 (_dur' m) t
             in t - pp0
        f (x,y) = let x' = x - t'
                      y' = y - t'
                  in if x' < 0 then (x'+_sup' m, y'+_sup' m)
                     else (x',y')
late' t m = sortMuseq' $ over vec' (V.map $ over _1 f) m
  where t' = let pp0 = prevPhase0 0 (_dur' m) t
             in t - pp0
        f (x,y) = let x' = x + t'
                      y' = y + t'
                  in if x' >= _sup' m then (x'-_sup' m, y'-_sup' m)
                     else (x',y')

fast, slow, dense, sparse :: Rational -> Museq a -> Museq a
fast d m = let f = (/d)
  in over dur f $ over sup f $ over vec (V.map $ over _1 f) $ m
slow d m = let f = (*d)
  in over dur f $ over sup f $ over vec (V.map $ over _1 f) $ m
dense d m = let f = (/d)
  in              over sup f $ over vec (V.map $ over _1 f) $ m
sparse d m = let f = (*d)
  in              over sup f $ over vec (V.map $ over _1 f) $ m

fast',slow',dense',sparse' :: Rational -> Museq' a -> Museq' a
fast' d m = let f = (/d)
                g (x,y) = (f x, f y)
  in over dur' f $ over sup' f $ over vec' (V.map $ over _1 g) $ m
slow' d m = let f = (*d)
                g (x,y) = (f x, f y)
  in over dur' f $ over sup' f $ over vec' (V.map $ over _1 g) $ m
dense' d m = let f = (/d)
                 g (x,y) = (f x, f y)
  in              over sup' f $ over vec' (V.map $ over _1 g) $ m
sparse' d m = let f = (*d)
                  g (x,y) = (f x, f y)
  in              over sup' f $ over vec' (V.map $ over _1 g) $ m

-- | I'm not sure what a fractional rotation means, so I have not tested it.
rotate, rep :: Rational -> Museq a -> Museq a -- the name `repeat` is taken
rotate t = fast t . sparse t
rep n = slow n . dense n


-- | = _ -> Museq Msg -> Museq Msg
overParams :: [(ParamName, Float -> Float)] -> Museq Msg -> Museq Msg
overParams fs mq = fmap change mq
  where mp = M.fromList fs
        change :: Msg -> Msg
        change (param,val) = ( param
                             , maybe val ($val) $ M.lookup param mp )

switchParams :: [(ParamName, ParamName)] -> Museq Msg -> Museq Msg
switchParams fs mq = fmap change mq where
  mp = M.fromList fs
  change msg@(param,_) = over _1 f msg where
    f = maybe id const $ M.lookup param mp

keepParams :: [ParamName] -> Museq Msg -> Museq Msg
keepParams ps = over vec $ V.filter $       f . fst . snd
  where f = flip S.member $ S.fromList ps

dropParams :: [ParamName] -> Museq Msg -> Museq Msg
dropParams ps = over vec $ V.filter $ not . f . fst . snd
  where f = flip S.member $ S.fromList ps


-- | = _ -> Museq' Msg -> Museq' Msg
overParams' :: [(ParamName, Float -> Float)] -> Museq' Msg -> Museq' Msg
overParams' fs mq = fmap change mq
  where mp = M.fromList fs
        change :: Msg -> Msg
        change (param,val) = ( param
                             , maybe val ($val) $ M.lookup param mp )

switchParams' :: [(ParamName, ParamName)] -> Museq' Msg -> Museq' Msg
switchParams' fs mq = fmap change mq where
  mp = M.fromList fs
  change msg@(param,_) = over _1 f msg where
    f = maybe id const $ M.lookup param mp

keepParams' :: [ParamName] -> Museq' Msg -> Museq' Msg
keepParams' ps = over vec' $ V.filter $       f . fst . snd
  where f = flip S.member $ S.fromList ps

dropParams' :: [ParamName] -> Museq' Msg -> Museq' Msg
dropParams' ps = over vec' $ V.filter $ not . f . fst . snd
  where f = flip S.member $ S.fromList ps
