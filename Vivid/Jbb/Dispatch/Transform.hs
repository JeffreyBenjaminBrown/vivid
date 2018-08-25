{-# LANGUAGE ScopedTypeVariables, ViewPatterns #-}

module Vivid.Jbb.Dispatch.Transform
  (
  rev
  , early, late
  , fast, slow
  , dense, sparse
  , rotate, rep

  , overParams
  , switchParams
  , keepParams
  , dropParams
  )
where


import Control.Lens (over, _1, _2)
import Data.Fixed (div',mod')
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V

import Vivid.Jbb.Util
import Vivid.Jbb.Dispatch.Museq
import Vivid.Jbb.Dispatch.Types


-- todo ? sorting in `rev` is overkill; faster would be to move the
-- elements at time=1, if they exist, to time=0
rev :: Museq a -> Museq a -- the name "reverse" is taken
rev m = sortMuseq $ over vec g m where
  g = V.reverse . V.map (over _1 f) where
    s = _sup m
    f (x,y) = if x > 0
              then (s-x, (s-x) + (y-x))
              else (x,y)

-- todo ? sorting in `early` or `late` is overkill, similar to `rev`
early :: RDuration -> Museq a -> Museq a
early t m = sortMuseq $ over vec (V.map $ over _1 shift) m
  where shiftStart :: RTime -> RTime
        shiftStart rt = mod' (rt - t) (_sup m)
        shift :: (RTime,RTime) -> (RTime,RTime)
        shift (x,y) = let x' = shiftStart x in (x',x' + (y-x))
late t = early (-t)

fast,slow,dense,sparse :: Rational -> Museq a -> Museq a
fast d m = let f = (/ (RTime d))
               g (x,y) = (f x, f y)
  in over dur f $ over sup f $ over vec (V.map $ over _1 g) $ m
slow d m = let f = (* (RTime d))
               g (x,y) = (f x, f y)
  in over dur f $ over sup f $ over vec (V.map $ over _1 g) $ m
dense d m = let f = (/ (RTime d))
                g (x,y) = (f x, f y)
  in              over sup f $ over vec (V.map $ over _1 g) $ m
sparse d m = let f = (* (RTime d))
                 g (x,y) = (f x, f y)
  in              over sup f $ over vec (V.map $ over _1 g) $ m

-- | I'm not sure what a fractional rotation means, so I have not tested it.
rotate, rep :: Rational -> Museq a -> Museq a -- name `repeat` is taken
rotate t = fast t . sparse t
rep n = slow n . dense n


-- | = _ -> Museq MapMsg -> Museq MapMsg
overParams :: [(ParamName, Float -> Float)] -> Museq MapMsg -> Museq MapMsg
overParams fs = fmap $ M.mapWithKey g
  where g k v = maybe v ($v) $ M.lookup k $ M.fromList fs

switchParams :: [(ParamName, ParamName)] -> Museq MapMsg -> Museq MapMsg
switchParams fs = fmap $ M.mapKeys g where
  g :: ParamName -> ParamName
  g k = maybe k id $ M.lookup k $ M.fromList fs

keepParams :: [ParamName] -> Museq MapMsg -> Museq MapMsg
keepParams ps = over vec $ V.filter (not . null . snd)
                . (V.map $ over _2 $ flip M.restrictKeys $ S.fromList ps)

dropParams :: [ParamName] -> Museq MapMsg -> Museq MapMsg
dropParams ps = over vec $ V.filter (not . null . snd)
                . (V.map $ over _2 $ flip M.withoutKeys $ S.fromList ps)
