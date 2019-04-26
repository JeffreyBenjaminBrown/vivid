pat = mmho 6 $ pre2 "a"
  [ (0, m1 "freq" 0)
  , (1/2, m1 "freq" 1)
  , (1, m1 "freq" 2)
  , (2, m1 "freq" 3)
  , (3, m1 "freq" 4)
  , (4, m1 "freq" 5)
  , (5, m1 "on" 0) ]

scalePat = mmh 12 $ pre2 "a"
  [ ( 0, [0,2,3,5,7,9,11] )
  , ( 6, [0,2,4,5,7,8,10] ) ]

toScale = ops [("freq", (*) 300 . \p -> 2**(p/12))] . scale scalePat

ch "1" $ nBoop $ toScale $
  ops [("freq",((-) 12))] $
  pat
ch "2" $ nBoop $ toScale $
  ops [("freq",(+ 2))] $
  fast 2 pat
ch "3" $ nBoop $ toScale $
  ops [("freq",(+ 4))] $
  fast 4 $ early 2 $ pat
