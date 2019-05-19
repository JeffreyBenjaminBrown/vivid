p1 = fast 2 $ early 1 $ mmho 4 $ pre2 "a"
  [ (0, m1 "freq" 0)
  , (1, m1 "freq" 2)
  , (2, m1 "freq" 4)
  , (3, m1 "on"   0) ]
p2 f = mmho 1 $ [ ("a", 0, m1 "freq" f)
                , ("b", 0, m1 "freq" 0)
                ]
p3 = merge0 p1 $ stack2 (p2 3) (p2 10)
  
rootPat = slow 8 $ mmh 8 $ pre2 "a"
  [ ( 0, 0 )
  , ( 2, 5 )
  , ( 3, 0 )
  , ( 4, 7 )
  , ( 5, 5 )
  , ( 6, 0 )
  , ( 7, 7 ) ]

scalePat = slow 4 $ cat [dense 2 s1, dense 2 s2] where
  s1 = mmh 2 $ pre2 "a"
    [ ( 0 , dor2 )
    , ( 1 , loc6 ) ]
  s2 = mmh 2 $ pre2 "a"
    [ ( 0 , maj )
    , ( 1 , lyd7 ) ]

toScale = nBoop
          . ops [("freq", (*) 200 . \p -> 2**(p/12))]
          . root (slow 8 rootPat)
          . scale (fast 2 scalePat)

chAll $ mfl [
  ("1", toScale $ cat [p1,p3] )
  , ("2", toScale $ rev $ ops [("freq",(+4))] p3 )
  ]
