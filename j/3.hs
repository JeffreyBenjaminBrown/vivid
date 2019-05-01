d = 3
p1 = slow (d/4) $ mmho 4 $ pre2 "a"
  [ (0, m1 "freq" 0)
  , (1, m1 "freq" 2)
  , (2, m1 "freq" 4)
  , (3, m1 "on"   0) ]

p2 = slow d $ mmho 4 $ pre2 "a"
  [ (0, m1 "freq" 0)
  , (1, m1 "freq" 1)
  , (2, m1 "freq" 2)
  , (3, m1 "freq" 1) ]

scalePat = slow (d/2) $ mmh 2 $ pre2 "a"
  [ ( 0 , maj3 )
  , ( 1 , aol3 )
  ]

halfDur f = slow (d/2) $ mmh 2 $ pre2 "a"
  [ (0, id)
  , (1, f) ]

toScale = nBoop
          . meta (slow 4 $ halfDur $ ops [("freq",(*) (4/3))])
          . ops [("freq", (*) 200 . \p -> 2**(p/12))]
          . scale scalePat

chAll $ mfl [
  ("1", toScale $ ops [("amp",const 0.1)] $
        meta (slow 2 $ halfDur $ ops [("on",const 0)]) $
        meta (halfDur rev) $ merge0 p1 p2)
  , ("2", toScale $ ops [("freq",((-) 12))] $ fast 2 $
          meta ( meta (slow 3 $ halfDur $ early 2 . fast 3) $
                 halfDur $ ops [("on",const 0)] )
          p1 )
  , ("3", toScale $ ops [("freq",((-) 9))] $ fast 2 $
          meta ( meta (slow 3 $ halfDur $ early 1 . fast 3) $
                 fast 2 $ halfDur $ ops [("on",const 0)] )
          p1 ) ]
