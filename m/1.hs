p f g h = mmho 7 $ pre2 "a"
  [ (0, m1 "freq" f)
  , (1, m1 "freq" $ f*g)
  , (2, m1 "freq" $ f*h/2)
  , (3, m1 "on" 0)
  , (4, m1 "freq" $ f*g)
  , (5, m1 "freq" f)
  , (6, m1 "on" 0) ]

m = mmh 7 [ trip "a" 0 id
          , trip "a" 3 $ early (1/2) ]

--ch "2" $ nBoop $       fast 2 $ p 300 (12/7) (7/6)
--ch "3" $ nBoop $          fast 4 $  p 600 (12/11) (9/5)

chAll $ mfl [
  ("1", nBoop $                  p 200 (11/8) (8/5) )
  , ("2", nBoop $ meta m $ fast 2 $  p 350 (12/9) (9/6) )
  , ("3", nBoop $ fast 4 $ cat [ p 600 (12/11) (9/5)
                               , p 500 (13/11) (9/5) ] ) ]
