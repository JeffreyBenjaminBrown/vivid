dur0 = 6
pat = mmho dur0 $ pre2 "a"
  [ (0, m1 "freq" 0)
  , (1/2, m1 "freq" 1)
  , (1, m1 "freq" 2)
  , (2, m1 "freq" 3)
  , (3, m1 "freq" 4)
  , (4, m1 "freq" 5)
  , (5, m1 "on" 0) ]

scalePat = mmh (4*dur0) $ pre2 "a"
  [ ( 0     , maj3 )
  , ( dur0  , dim  )
  , ( 2*dur0, aol3 )
  , ( 3*dur0, aug  ) ]

toScale = nBoop
          . ops [("freq", (*) 300 . \p -> 2**(p/12))]
          . scale scalePat

chAll $ mfl [
    ("1", toScale $ ops [("freq",((-) 12))] $ rev pat)
  , ("2", toScale $ ops [("freq",(+ 2))] $ fast 2 pat)
  , ("3", toScale $ ops [("freq",(+ 4))] $ fast 4 $ early 2 $ pat) ]
