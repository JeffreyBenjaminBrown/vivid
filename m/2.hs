pat f g h = mmho 4 $ pre2 "a"
  [ (0, m1 "freq" f)
  , (1, m1 "freq" $ f*g)
  , (2, m1 "freq" $ f*h)
  , (3, m1 "on" 0) ]

pit f = mmho 1 $ pre2 "a" [ (0, m1 "freq" f) ]

mRyt = mmh 7 [ trip "a" 0 $ fast 2
             , trip "a" 3 $ early 1 ]

ch "1" $ nBoop $ pat 125 (5/4) (3/2)

ch "2" $ nBoop
  (mergea
    (pat 250 (5/4) (3/2))
    (stacks
      [ slow 2 $              pat 1 (10/8) (10/7)
      , early 2 $ fast 2 $ pat 3 (9/8) (9/6)
      , meta (slow 2 $ mRyt) $ fast 4 $ pat (5/2) (5/4) (9/6) ] ) )

ch "3" $ nBoop $ meta mRyt $ pat 750 (5/4) (3/2)
