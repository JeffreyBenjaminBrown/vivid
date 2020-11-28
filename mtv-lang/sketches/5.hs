p0 = mmho 4 $ [ ("a",0,m1 "freq" 0)
              , ("b",0,m1 "freq" 2)
              , ("c",0,m1 "freq" 4)
              , ("d",0,m1 "freq" 6)
              ]
  
p1 = mmho 4 $ pre2 "a"
  [ (0, m1 "freq" 0)
  , (1+2/3, m1 "freq" 2)
  , (2, m1 "freq" 4)
  , (3, m1 "on"   0) ]

p2 = mmho 4 $ pre2 "a"
  [ (0, m1 "freq" 0)
  , (1, m1 "freq" 3)
  , (2+2/3, m1 "freq" 6)
  , (3, m1 "on"   0) ]

t1 = mmh 6 $ pre2 "a"
  [ (0, id)
  , (1,fast 2)
  , (2,id)
  , (3, early 1)
  , (4, id)
  , (5, fast 2 . early 1) ]

fe = fast 2 . early 1

r1 = mmh 5 $ pre2 "a"
  [ ( 0, 0 )
  , ( 3, 3 ) ]

s1 = mmh 3 $ pre2 "a"
  [ ( 0 , dor2 )
  , ( 1 , loc6 )
  , ( 2 , [0,4,2,4,7,11] ) ]

toScale = nBoop .
          ops [("freq", (*) 200 . \p -> 2**(p/12))] .
          scale 12 (slow 2 s1) . root (slow 4 r1) .
          merge0 p0

--chAll $ mfl [
--    ("1", toScale $ ops [("freq",(+) 2)] $
--          meta (fast 2 t1) $ cat [sparse 2 p1, fast 2 p2] )
--  , ("2", toScale $ ops [("freq",(-) 8)] $
--          meta t1          $ cat [dense 2 p1, p2] )
--  ]

chAll $ mfl [
    ("1", toScale $ cat                        [sparse 2 $ fe p1, fe p2] )
  , ("2", toScale $ ops [("freq",(-) 7)] $ cat [dense  2 $ fe p1, fe p2] )
  ]
