-- I had to write this twice; therefore keeping it.

arcIO :: Show a => Time -> Duration -> Time -> Time
    -> Museq a -> IO [(Time, a)]
arcIO time0 tempoPeriod from to m = do
  let period = tempoPeriod * fromRational (_sup m)
      rdv = V.map fst $ _vec $ const () <$> m :: V.Vector RTime
      firstPhase0 = prevPhase0 time0 period from
      toAbsoluteTime :: RTime -> Time
      toAbsoluteTime rt = fromRational rt * tempoPeriod + firstPhase0
  putStrLn $ "\n\nperiod: " ++ show period
    ++ "\nrdv: " ++ show rdv
    ++ "\nfirstPhase0: " ++ show (firstPhase0 - time0)
  x <- arcFoldIO 0 period rdv time0 from to m
  return $ map (over _1 toAbsoluteTime) x

arcFoldIO :: Show a => Int -> Duration -> V.Vector RelDuration
  -> Time -> Time -> Time -- ^ the same three `Time` arguments as in `arc`
  -> Museq a -> IO [(RTime, a)]
arcFoldIO cycle period rdv time0 from to m = do
  putStrLn $ "\ncycle: " ++ show cycle
    ++ "\nperiod: " ++ show period
    ++ "\nrdv: " ++ show rdv
    ++ "\ntime0: " ++ show time0
    ++ "\nfrom: " ++ show from
    ++ "\nto: " ++ show to
  if from >= to then return [] -- todo ? Be sure of boundary condition
    else do
    let
      pp0 = prevPhase0 time0 period from
      fromInCycles = toRational $ (from - pp0) / period
      toInCycles   = toRational $ (to   - pp0) / period
      startOrOOBIndex = firstIndexGTE compare rdv (fromInCycles * _sup m)
    putStrLn $ "\npp0: " ++ show (pp0 - time0)
      ++ "\nfromInCycles: " ++ show (fromRational fromInCycles :: Float)
      ++ "\ntoInCycles: " ++ show (fromRational toInCycles :: Float)
      ++ "\nstartOrOOBIndex: " ++ show startOrOOBIndex
    if startOrOOBIndex >= V.length rdv
      then do let nextFrom = if pp0 + period > from
                    then pp0 + period -- floating point error makes this 
                    else pp0 + 2*period -- else staatement necessary
              putStrLn $ "\nnextFrom: " ++ show nextFrom
              arcFoldIO (cycle+1) period rdv time0 nextFrom to m
      else do
      let startIndex = startOrOOBIndex
          endIndex = lastIndexLTE compare' rdv (toInCycles * _sup m) where
            compare' x y = if x < y then LT else GT -- to omit the endpoint
          eventsThisCycle = V.toList
             $ V.map (over _1 (+(_sup m * fromIntegral cycle)))
             $ V.slice startIndex (endIndex-startIndex) $ _vec m
      putStrLn $ "\nstartIndex: " ++ show startIndex
        ++ "\nendIndex: " ++ show endIndex
        ++ "\neventsThisCycle: " ++ show eventsThisCycle
      x <- arcFoldIO (cycle+1) period rdv time0 (pp0 + period) to m
      return $ eventsThisCycle ++ x
