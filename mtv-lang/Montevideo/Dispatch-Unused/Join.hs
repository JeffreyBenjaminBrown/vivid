mergeIO :: forall a. Show a =>
  (a -> a -> a) -> Museq a -> Museq a -> IO (Museq a)
mergeIO op x y = do
  let tbr = timeForBoth_toAppearToFinish x y
      xs, ys, xps, yps :: [((RTime,RTime),a)]
      xs = concatMap V.toList $ unsafeExplicitReps tbr x
      ys = concatMap V.toList $ unsafeExplicitReps tbr y
      bs = boundaries $ map fst $ xs ++ ys :: [RTime]
      xps = partitionAndGroupEventsAtBoundaries bs xs
      yps = partitionAndGroupEventsAtBoundaries bs ys
  putStrLn $ "\ntbr: " ++ show tbr
    ++ "\nbs: " ++ show bs
    ++ "\nxs: " ++ show xs
    ++ "\nys: " ++ show ys
    ++ "\nxps: " ++ show xps
    ++ "\nyps: " ++ show yps
  evs <- alignAndMergeIO op xps yps
  return $ Museq { _dur = _dur x -- arbitrary
                 , _sup = tbr
                 , _vec = V.fromList evs}

alignAndMergeIO,mergeEventsIO :: forall a.
  (a -> a -> a) -> [Ev a] -> [Ev a] -> IO [Ev a]
alignAndMergeIO _ [] _ = return []
alignAndMergeIO _ _ [] = return []
alignAndMergeIO op aEvs@((arcA,_):aEvsRest)  bEvs@((arcB,_):bEvsRest)
  | arcA <  arcB = alignAndMergeIO op aEvsRest bEvs
  | arcB <  arcA = alignAndMergeIO op aEvs bEvsRest
  | arcA == arcB = mergeEventsIO op aEvs bEvs
mergeEventsIO op ((arc,a):aEvs) bEvs = do
  let bEvsMatch = takeWhile ((== arc) . fst) bEvs
      merged = over _2 (op a) <$> bEvsMatch
  x <- alignAndMergeIO op aEvs bEvs
  return $ merged ++ x
