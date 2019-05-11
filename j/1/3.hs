dur0 = 6
evs = [ (0, m1 "freq" 0)
      , (1/2, m1 "freq" 1)
      , (1, m1 "freq" 2)
      , (2, m1 "freq" 3)
      , (3, m1 "freq" 4)
      , (4, m1 "freq" 5)
      , (5, m1 "on" 0) ]
pat = mmho dur0 $ pre2 "a" evs
hatPat = mmrt1 6 $ map (_2 %~ f) evs where
  f :: M.Map String Float -> Sample
  f = maybe SampleSl_b (const SampleHl_cg) . M.lookup "freq"
kickSnarePat = mmrt1 2 [ (0, SampleKd)
                       , (1, SampleSp_t) ]

scalePat = mmh (4*dur0) $ pre2 "a"
  [ ( 0     , maj3 )
  , ( dur0  , dim  )
  , ( 2*dur0, aol3 )
  , ( 3*dur0, aug  ) ]

revPat = mmh (2*dur0) $ pre2 "a"
  [ (0, id)
  , (dur0, rev) ]

toScale = nBoop
          . ops [("freq", (*) 300 . \p -> 2**(p/12))]
          . scale scalePat

chAll $ mfl [
    ("1", toScale $ ops [("freq",((-) 12))] $                 meta revPat pat)
  , ("2", toScale $ ops [("freq",(+ 2))] $ fast 2 $           meta revPat pat)
  , ("3", toScale $ ops [("freq",(+ 4))] $ fast 4 $ early 2 $ meta revPat pat)
  , ("4", stacks [ fast 2 $ early (1/4) $ kickSnarePat
                 , append hatPat (rev $ fast 4 hatPat)
                 , fast 4 $ early (1/2) $ meta revPat $ hatPat
                 , kickSnarePat ] )
  ]
