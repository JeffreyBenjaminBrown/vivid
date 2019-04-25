p f g = mmho 3 $ pre2 "a"
  [ (0, m1 "freq" f)
  , (1, m1 "freq" $ f*g)
  , (2, m1 "on" 0) ]

m = mmh 9
  [ trip "a" 0 id
  , trip "a" 3 $ fast' 2
  , trip "b" 3 $ fast' 4
  -- Wart: For safety, to prevent hanging notes,
  -- both voices must be turned off explicitly, I think.
  , trip "a" 7 $ offs
  , trip "b" 7 $ offs
  ]

m2 = mmh 9 $ pre2 "a" [ (0,id), (5,early' (1/2)) ]

replaceAll' disp $ M.fromList
  [ ("1", Note' Boop <$> meta' m
          (mergea'
           (p 400 $ 5/4)
           (meta' m2 $ fast' 2 $ p 1 $ 3/5)) )
  , ("2", Note' Boop <$> meta' (fast' 2 m)
          (mergea'
           (p 300 $ 10/11)
           (fast' 2 $ p 1 $ 3/7)) ) ]
