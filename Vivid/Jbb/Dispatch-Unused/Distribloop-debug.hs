    --  deepseq (time0, tempoPeriod, museqsMap, reg, now, np0, startRender)
    --    (return evs)

    --  let rNow = now - time0
    --      rNp0 = np0 - time0
    --      rStartRender = startRender - time0
    --      rEvs = flip map evs $ over _1 (+(-time0))

    --  putStrLn $ "\nNow: " ++ show rNow ++ "\nnp0: " ++ show rNp0
    --    ++ "\nstartRender: " ++ show rStartRender
    --    ++ "\ntempoPeriod: " ++ show tempoPeriod
    --    ++ "\nmuseqsMap: " ++ concatMap ((++"\n") . show) (M.toList $ museqsMap)

    --  putStrLn $ "\nlength evs: " ++ show (length evs) ++ "\nevs: "
    --    ++ concatMap (\(t,a) -> "\n" ++ show (t-time0) ++ ": " ++ show a) evs
    --    ++ "\nThat's all of them?\n"
