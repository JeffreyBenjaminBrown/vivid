{-# LANGUAGE ScopedTypeVariables, ViewPatterns #-}

module Vivid.Jbb.Dispatch.Join
  (
  append
  , cat
  , stack
  )
where

import Control.Lens (over, _1)
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
      ixs, iys :: [(Int,V.Vector ((RTime,RTime),a))]
      ixs = zip [0..] $ unsafeExplicitReps (durs * _dur x) x
      iys = zip [1..] $ unsafeExplicitReps (durs * _dur y) y
        -- ixs uses a 0 because it starts with no ys before it
        -- iys uses a 1 because it starts with 1 (_dur x) worth of x before it

      -- next, space out the xs to make room for the ys, and vice versa
      adjustx, adjusty :: (Int,V.Vector ((RTime,RTime),a))
                       ->      V.Vector ((RTime,RTime),a)
      adjustx (idx,v) = V.map f v where
        f = over _1 (\(x,y) -> (g x, g y)) where
          g = (+) $ fromIntegral idx * _dur y
      adjusty (idx,v) = V.map f v where
        f = over _1 (\(x,y) -> (g x, g y)) where
          g = (+) $ fromIntegral idx * _dur x
      xs, ys :: [V.Vector ((RTime,RTime),a)]
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

--merge :: (a -> a) -> Museq a -> Museq a -> Museq a -- TODO
--merge op x y = let let t = timeForBothToRepeat x y
--                       xs = concat $ unsafeExplicitReps t x
--                       ys = concat $ unsafeExplicitReps t y
