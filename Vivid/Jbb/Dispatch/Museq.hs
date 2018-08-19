-- | = Mostly analysis

module Vivid.Jbb.Dispatch.Museq (
    timeToPlayThrough
  , supsToPlayThrough
  , dursToPlayThrough
  , timeToRepeat
  , supsToRepeat
  , dursToRepeat

  , museqSynths
  , museqsDiff

  , sortMuseq
  , museqIsValid
  , findNextEvents

  , arc
  -- , arcIO
  ) where

import Control.Lens ((^.),(.~),(%~),_1,_2,over)
import Control.Monad.ST
import Data.List ((\\))
import qualified Data.Map as M
import qualified Data.Vector as V
import           Data.Vector ((!))
import Data.Vector.Algorithms.Intro (sortBy)

import Vivid.Jbb.Dispatch.Types
import Vivid.Jbb.Util
import Vivid.Jbb.Synths (SynthDefEnum(Boop))


-- | = Figuring out when a Museq will repeat.
-- There are two senses in which a Museq can repeat. One is that
-- it sounds like it's repeating. If _dur = 2 and _sup = 1, then
-- it sounds like it's repeating after a single _sup.
-- The *ToRepeat functions below use that sense.
--
-- The other sense is that the Museq has cycled through what it
-- "is supposed to cycle through". (This is useful when `append`ing Museqs.)
-- If _dur = 2 and _sup = 1, it won't have played all the way through
-- until _dur has gone by, even though a listener hears it doing the same
-- thing halfway through that _dur.
-- The *ToPlayThrough functions below use that sense.
--
-- The results of the two families only differ when _sup divides _dur.
--
-- I could have used the *PlayThrough functions everywhere, but
-- in some situations that would waste space. For an example of one,
-- see in Tests.testStack the assertion labeled
-- "stack, where timeToRepeat differs from timeToPlayThrough".

timeToPlayThrough :: Museq a -> Rational
timeToPlayThrough m = lcmRatios (_sup m) (_dur m)

supsToPlayThrough :: Museq a -> Rational
supsToPlayThrough m = timeToPlayThrough m / _sup m

dursToPlayThrough :: Museq a -> Rational
dursToPlayThrough m = timeToPlayThrough m / _dur m

timeToRepeat :: Museq a -> Rational
timeToRepeat m = let x = lcmRatios (_sup m) (_dur m)
  in if x == _dur m then _sup m else x

supsToRepeat :: Museq a -> Rational
supsToRepeat m = timeToRepeat m / _sup m

dursToRepeat :: Museq a -> Rational
dursToRepeat m = timeToRepeat m / _dur m


-- | Given a Museq, find the synths it uses.
museqSynths :: Museq Action -> [(SynthDefEnum, SynthName)]
museqSynths = map (actionSynth . snd) . V.toList . _vec

-- | Given an old set of Museqs and a new one, figure out
-- which synths need to be created, and which destroyed.
-- PITFALL: Both resulting lists are ordered on the first element,
-- likely differing from either of the input maps.
museqsDiff :: M.Map MuseqName (Museq Action)
           -> M.Map MuseqName (Museq Action)
           -> ([(SynthDefEnum, SynthName)],
               [(SynthDefEnum, SynthName)])
museqsDiff old new = (toFree,toCreate) where
  oldMuseqs = M.elems old :: [Museq Action]
  newMuseqs = M.elems new :: [Museq Action]
  oldSynths = unique $ concatMap museqSynths oldMuseqs
  newSynths = unique $ concatMap museqSynths newMuseqs
  toCreate = newSynths \\ oldSynths
  toFree = oldSynths \\ newSynths


-- | = Sort a Museq
sortMuseq :: Museq a -> Museq a
sortMuseq = vec %~
  \v -> runST $ do v' <- V.thaw v
                   let compare' ve ve' = compare (fst ve) (fst ve')
                   sortBy compare' v'
                   V.freeze v'

-- | A valid Museq m is sorted on start time, has (relative) duration > 0,
-- and all actions at time < _sup m.
museqIsValid :: Eq a => Museq a -> Bool
museqIsValid mu = and [a,b,c,d] where
  a = if V.length (_vec mu) == 0 then True
      else fst (V.last $ _vec mu) < _sup mu
  b = mu == sortMuseq mu
  c = _dur mu > 0
  d = _sup mu > 0

-- todo ? `findNextEvents` could be ~2x faster by using binarySearchRByBounds
-- instead of binarySearchR, to avoid searching the first part
-- of the vector again.
-- | Returns a list of actions and the time remaining until they start.
findNextEvents :: Time -> Duration -> Time
               -> Museq Action -> (Duration, [Action])
findNextEvents time0 tempoPeriod now museq =
  let period = tempoPeriod * fromRational (_sup museq)
      pp0 = prevPhase0 time0 period now
      relNow = toRational $ (now - pp0) / period
      vecLen = V.length $ _vec museq
      uv = _vec $ const () <$> museq :: V.Vector (RelDuration,())
      compare' :: (RelDuration, a) -> (RelDuration, a) -> Ordering
      compare' ve ve' = compare (fst ve) (fst ve')
      startOrOOB = firstIndexGTE  compare' uv (relNow, ())
      start = if startOrOOB < vecLen then startOrOOB else 0
      end =     lastIndexLTE  compare' uv (uv ! start)
      relTimeOfNextEvent = if startOrOOB == start
                           then        fst $ uv ! start
                           else (+1) $ fst $ uv ! 0
      timeUntilNextEvent =
        fromRational relTimeOfNextEvent * period + pp0 - now
  in ( timeUntilNextEvent
     , map snd $ V.toList $ V.slice start (end - start) $ _vec museq )

-- todo ? `arc` could be ~2x faster by using binarySearchRByBounds
-- instead of binarySearchR, to avoid searching the first part
-- of the vector again.
-- | Finds the events in [from,to), and when they should start,
-- in relative time units.
arc :: Time -> Duration -> Time -> Time
    -> Museq a -> [(Time, a)]
arc time0 tempoPeriod from to m =
  let period = tempoPeriod * fromRational (_sup m)
      rdv = V.map fst $ _vec $ const () <$> m :: V.Vector RelDuration
      firstPhase0 = prevPhase0 time0 period from
      toAbsoluteTime :: RTime -> Time
      toAbsoluteTime rt = fromRational rt * tempoPeriod + firstPhase0
   in map (over _1 toAbsoluteTime) $ arcFold 0 period rdv time0 from to m

arcFold :: Int -> Duration -> V.Vector RelDuration
  -> Time -> Time -> Time -- ^ the same three `Time` arguments as in `arc`
  -> Museq a -> [(RTime, a)]
arcFold cycle period rdv time0 from to m = 
  if from >= to
  then [] -- todo ? Be sure of boundary condition
  else let
    pp0 = prevPhase0 time0 period from
    relFrom = toRational $ (from - pp0) / period
    relTo   = toRational $ (to   - pp0) / period
    startOrOOB = firstIndexGTE compare rdv (relFrom * _sup m)
  in if startOrOOB >= V.length rdv
     then let nextFrom = if pp0 + period > from
                         then pp0 + period
                         else pp0 + 2*period
  -- todo ? I know `nextFrom` (above) fixes the following bug,
    -- but why is it needed?
    -- The bug: Evaluate the following two statements. The second hangs.
      -- m = Museq {_dur = 1 % 6, _sup = 1 % 6, _vec = V.fromList [(1 % 24,Send Boop "3" ("amp",0.0)),(1 % 8,Send Boop "3" ("freq",600.0)),(1 % 8,Send Boop "3" ("amp",0.4))]}
      -- arc 0 1 8 9 m
          in arcFold (cycle+1) period rdv time0 nextFrom to m
     else let start = startOrOOB
              end = lastIndexLTE compare' rdv (relTo * _sup m) where
                compare' x y
                  = if x < y then LT else GT -- to omit the endpoint
              eventsThisCycle = V.toList
                $ V.map (over _1 (+(_sup m * fromIntegral cycle)))
                $ V.slice start (end-start) $ _vec m
          in eventsThisCycle
             ++ arcFold (cycle+1) period rdv time0 (pp0 + period) to m

-- | = Those same functions, as IO.
-- Keeping just in case I'll need to debug it again.

--arcIO :: Show a
--      => Time -> Duration -> Time -> Time
--      -> Museq a -> IO [(Time, a)]
--arcIO time0 tempoPeriod from to m = do
--  let period = tempoPeriod * fromRational (_sup m)
--      rdv = V.map fst $ _vec $ unitMuseq m :: V.Vector RelDuration
--      firstPhase0 = prevPhase0 time0 period from
--      toAbsoluteTime :: RTime -> Time
--      toAbsoluteTime rt = fromRational rt * tempoPeriod + firstPhase0
--  map (over _1 toAbsoluteTime) <$> arcFoldIO 0 period rdv time0 from to m
--
--arcFoldIO :: Show a
--  => Int -> Duration -> V.Vector RelDuration
--  -> Time -> Time -> Time -- ^ the same three `Time` arguments as in `arc`
--  -> Museq a -> IO [(RTime, a)]
--arcFoldIO cycle period rdv time0 from to m = do
--  putStrLn $ "\n\ncycle: " ++ show cycle
--    ++ "\nperiod: " ++ show period
--    ++ "\nrdv: " ++ show (V.map ((+(-time0)) . fromRational) rdv)
--    ++ "\nfrom: " ++ show (from - time0)
--    ++ "\nto: " ++ show (to - time0)
--    ++ "\nmuseqs: " ++ show m
--  if from >= to 
--    then return [] -- todo ? Be sure of boundary condition
--    else do 
--    let pp0 = prevPhase0 time0 period from
--        relFrom = toRational $ (from - pp0) / period
--        relTo   = toRational $ (to   - pp0) / period
--        startOrOOB = firstIndexGTE compare rdv (relFrom * _sup m)
--    putStrLn $ "\n\npp0: " ++ show pp0
--      ++ "\nrelFrom: " ++ show relFrom
--      ++ "\nrelTo: " ++ show relTo
--      ++ "\nstartOrOOB: " ++ show startOrOOB
--    if startOrOOB >= V.length rdv
--      then do putStrLn "first branch"
--              let nextFrom = if pp0 + period > from
--                             then pp0 + period
--                             else pp0 + 2*period
--              arcFoldIO (cycle+1) period rdv time0 nextFrom to m
--      else do
--      let start = startOrOOB
--          end = lastIndexLTE compare' rdv (relTo * _sup m) where
--            compare' x y =
--              if x < y then LT else GT -- to omit the endpoint
--          eventsThisCycle = V.toList
--            $ V.map (over _1 (+(_sup m * fromIntegral cycle)))
--            $ V.slice start (end-start) $ _vec m
--      next <- arcFoldIO (cycle+1) period rdv time0 (pp0 + period) to m
--      return $ eventsThisCycle ++ next
