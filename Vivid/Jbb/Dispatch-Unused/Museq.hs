-- Keep this!
-- I had to write this twice; kept it after the second time;
-- ended up then using it once.

arcIO' :: forall l a. (Show a, Show l)
       => Time -> Duration -> Time -> Time
       -> Museq' l a -> IO [AbsEv' l a]
arcIO' time0 tempoPeriod from to m = do
  let period = tempoPeriod * tr (_sup' m) :: Duration
      startVec = V.map (view evStart) $ _vec' $ m :: V.Vector RTime
      latestPhase0 = prevPhase0 time0 period from :: Time
        -- it would be natural to start here, but long events from
        -- earlier cycles could carry into now, so we must back up
      earlierFrom = latestPhase0 - tr (longestDur' m) * tempoPeriod :: Time
      oldestRelevantCycle = div' (earlierFrom - latestPhase0) period :: Int
      correctAbsoluteTimes :: (Time,Time) -> (Time,Time)
        -- HACK: the inputs ought to be RTimes,
        -- but then I'd be switching types, from Ev' to AbsEv'
      correctAbsoluteTimes (a,b) = (f a, f b) where
        f rt = tr rt * tempoPeriod + latestPhase0
      chopStarts :: (Time,Time) -> (Time,Time)
      chopStarts = over _1 $ max from
      chopEnds :: (Time,Time) -> (Time,Time)
      chopEnds = over _2 $ min to
      dropImpossibles :: [AbsEv' l a] -> [AbsEv' l a]
        -- Because chopStarts can leave an old event starting after it ended.
      dropImpossibles = filter $ uncurry (<=) . view absEvArc
      futzTimes :: Ev' l a -> AbsEv' l a
      futzTimes ev = over absEvArc f $ toAbsEv ev
        where f = chopEnds . chopStarts . correctAbsoluteTimes
  putStrLn $ "\n\nperiod: " ++ show period
    ++ "\nstartVec: " ++ show startVec
    ++ "\nlatestPhase0: " ++ show latestPhase0
    ++ "\nearlierFrom: " ++ show earlierFrom
    ++ "\noldestRelevantCycle: " ++ show oldestRelevantCycle
  x <- _arcFoldIO' oldestRelevantCycle period startVec time0 earlierFrom to m
  return $ dropImpossibles $ map futzTimes x

_arcFoldIO' :: forall l a. (Show a, Show l)
  => Int -> Duration -> V.Vector RTime
  -> Time -> Time -> Time -- ^ the same three `Time` arguments as in `arc`
  -> Museq' l a -> IO [Ev' l a]
_arcFoldIO' cycle period startVec time0 from to m = do
  putStrLn $ "\n\n---Starting arcFoldIO---':"
    ++ "\ncycle: " ++ show cycle
    ++ "\nperiod: " ++ show period
    ++ "\ntime0: " ++ show time0
    ++ "\nfrom: " ++ show from
    ++ "\nto: " ++ show to
  if from >= to then return [] -- todo ? Be sure of `arc` boundary condition
    else do
    let pp0 = prevPhase0 time0 period from :: Time
        fromInCycles = fr $ (from - pp0) / period :: RTime
        toInCycles   = fr $ (to   - pp0) / period :: RTime
        startOrOOBIndex =
          firstIndexGTE compare startVec $ fromInCycles * _sup' m :: Int
    putStrLn $ "\npp0: " ++ show pp0
      ++ "\nfromInCycles: " ++ show fromInCycles
      ++ "\ntoInCycles: " ++ show toInCycles
      ++ "\nstartOrOOBIndex: " ++ show startOrOOBIndex
    if startOrOOBIndex >= V.length startVec
--     then let nextFrom = if pp0 + period > from
-- -- todo ? delete
-- -- If `from = pp0 + period - epsilon`, maybe `pp0 + period <= from`.
-- -- Thus floating point error used to make this if-then statement necessary
-- -- Now that all times are Rational, it's probably unnecessary.
--                         then pp0 + period
--                         else pp0 + 2*period
--          in _arcFold (cycle+1) period startVec time0 nextFrom to m
      then do putStrLn $ "nothing to render this cycle"
              _arcFoldIO' (cycle+1) period startVec time0 (pp0 + period) to m
      else do
      let startIndex = startOrOOBIndex :: Int
          endIndex = lastIndexLTE compare' startVec
                     $ toInCycles * _sup' m :: Int
             where compare' x y =
                     if x < y then LT else GT -- to omit the endpoint
          eventsThisCycle = V.toList
             $ V.map (over evEnd   (+(_sup' m * fromIntegral cycle)))
             $ V.map (over evStart (+(_sup' m * fromIntegral cycle)))
             $ V.slice startIndex (endIndex-startIndex) $ _vec' m
      putStrLn $ "something to render this cycle: "
        ++ "\nstartIndex: " ++ show startIndex
        ++ "\nendIndex: " ++ show endIndex
        ++ "\neventsThisCycle: " ++ show eventsThisCycle
      x <- _arcFoldIO' (cycle+1) period startVec time0 (pp0 + period) to m
      return $ eventsThisCycle ++ x

arcIO :: forall a. Show a => Time -> Duration -> Time -> Time
     -> Museq a -> IO [((Time,Time), a)]
arcIO time0 tempoPeriod from to m = do
  let period = tempoPeriod * tr (_sup m) :: Duration
      rdv = V.map (fst . fst) $ _vec $ const () <$> m :: V.Vector RTime
      latestPhase0 = prevPhase0 time0 period from :: Time
      earlierFrom = from - tr (longestDur m) * tempoPeriod :: Time
      oldestRelevantCycle = div' (earlierFrom - latestPhase0) period :: Int
      toAbsoluteTime :: (RTime,RTime) -> (Time,Time)
      toAbsoluteTime (a,b) = (f a, f b) where
        f rt = tr rt * tempoPeriod + latestPhase0
      chopStarts :: (Time,Time) -> (Time,Time)
      chopStarts = over _1 $ max from
      chopEnds :: (Time,Time) -> (Time,Time)
      chopEnds = over _2 $ min to
  putStrLn $ "\n\nperiod: " ++ show period
    ++ "\nrdv: " ++ show rdv
    ++ "\nlatestPhase0: " ++ show latestPhase0
    ++ "\nearlierFrom: " ++ show earlierFrom
    ++ "\noldestRelevantCycle: " ++ show oldestRelevantCycle
  x <- arcFoldIO oldestRelevantCycle period rdv time0 earlierFrom to m
  return $ map (over _1 $ chopEnds . chopStarts . toAbsoluteTime) x

arcFoldIO :: forall a. Show a => Int -> Duration -> V.Vector RTime
  -> Time -> Time -> Time -- ^ the same three `Time` arguments as in `arc`
  -> Museq a -> IO [((RTime,RTime), a)]
arcFoldIO cycle period rdv time0 from to m = do
  putStrLn $ "\n\n---Starting arcFoldIO---':"
    ++ "\ncycle: " ++ show cycle
    ++ "\nperiod: " ++ show period
    ++ "\ntime0: " ++ show time0
    ++ "\nfrom: " ++ show from
    ++ "\nto: " ++ show to
  if from >= to then return [] -- todo ? Be sure of `arc` boundary condition
    else do
    let pp0 = prevPhase0 time0 period from :: Time
        fromInCycles = fr $ (from - pp0) / period :: RTime
        toInCycles   = fr $ (to   - pp0) / period :: RTime
        startOrOOBIndex =
          firstIndexGTE compare rdv $ fromInCycles * _sup m :: Int
    putStrLn $ "\npp0: " ++ show pp0
      ++ "\nfromInCycles: " ++ show fromInCycles
      ++ "\ntoInCycles: " ++ show toInCycles
      ++ "\nstartOrOOBIndex: " ++ show startOrOOBIndex
    if startOrOOBIndex >= V.length rdv
--     then let nextFrom = if pp0 + period > from
--    todo ? delete
-- -- If `from = pp0 + period - epsilon`, maybe `pp0 + period <= from`.
-- -- Thus floating point error used to make this if-then statement necessary
-- -- Now that all times are Rational, it's probably unnecessary.
--                         then pp0 + period
--                         else pp0 + 2*period
--          in arcFold (cycle+1) period rdv time0 nextFrom to m
      then do putStrLn $ "nothing to render this cycle"
              arcFoldIO (cycle+1) period rdv time0 (pp0 + period) to m
      else do
      let startIndex = startOrOOBIndex :: Int
          endIndex =
            lastIndexLTE compare' rdv (toInCycles * _sup m) :: Int
             where
             compare' x y = if x < y then LT else GT -- to omit the endpoint
          eventsThisCycle = V.toList
             $ V.map (over (_1._2) (+(_sup m * fromIntegral cycle)))
             $ V.map (over (_1._1) (+(_sup m * fromIntegral cycle)))
             $ V.slice startIndex (endIndex-startIndex) $ _vec m
      putStrLn $ "something to render this cycle: "
        ++ "\nstartIndex: " ++ show startIndex
        ++ "\nendIndex: " ++ show endIndex
        ++ "\neventsThisCycle: " ++ show eventsThisCycle
      x <- arcFoldIO (cycle+1) period rdv time0 (pp0 + period) to m
      return $ eventsThisCycle ++ x
