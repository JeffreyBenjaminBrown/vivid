{-# LANGUAGE ScopedTypeVariables, ViewPatterns #-}

module Vivid.Jbb.Dispatch.Transform (
  rev
  , early, late
  , fast, slow
  , dense, sparse
  , rotate, rep

  , overParams
  ) where

import Control.Lens (over, _1, _2)
import qualified Data.Map as M
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
        f x = if s-x < s then s-x else 0

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

fast, slow, dense, sparse :: Rational -> Museq a -> Museq a
fast d m = let f = (/d)
  in over dur f $ over sup f $ over vec (V.map $ over _1 f) $ m
slow d m = let f = (*d)
  in over dur f $ over sup f $ over vec (V.map $ over _1 f) $ m
dense d m = let f = (/d)
  in              over sup f $ over vec (V.map $ over _1 f) $ m
sparse d m = let f = (*d)
  in              over sup f $ over vec (V.map $ over _1 f) $ m

-- | I'm not sure what a fractional rotation means, so I have not tested it.
rotate, rep :: Rational -> Museq a -> Museq a -- the name `repeat` is taken
rotate t = fast t . sparse t
rep n = slow n . dense n

overParams :: [(ParamName, Float -> Float)] -> Museq Msg -> Museq Msg
overParams fs mq = fmap change mq
  where mp = M.fromList fs
        change :: Msg -> Msg
        change (param,val) = ( param
                             , maybe val ($val) $ M.lookup param mp )
