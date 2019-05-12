-- Hangs not forever, but for a long time --
-- unless I use the edit described at the end.

p1 = stack a b & dur .~ 4 where
  a = mmho 3 $ pre2 "a" [(0, m1 "freq" 0)
                        ,(1, m1 "freq" 2)]
  b = mmho 2 $ pre2 "b" [(0, m1 "on" 0)
                        ,(1, m1 "freq" 4)]

p2 :: Int -> Museq String Msg = \n ->
  mmho (fromIntegral n) $ pre2 "a" $
  zip (map RTime [0..]) $
  map (M.singleton "freq" . (+) 0) $
  map fromIntegral [0..n-1]

go = nBoop . toHz . rootScale rs where
  toHz = ops [("freq", (*) 200 . \p -> 2**(p/12))]
  rs = slow 12 $ mmh 3 $ pre2 "a" $ [ (0, (0, phr3))
                                    , (1, (4, lyd))
                                    , (2, (1, lyd7))
                                    ]

wave = slow 4 $ mmh 2 $ pre2 "a" $ zip (map RTime [0..]) $
       [id, early $ 1]

chAll $ mfl [
  ("1", go $ fast 2 $ merge0fa (slow 4 $ p2 4) $ merge0fa (p2 4) p1)
  , ("2", go $ fast 4 $ meta wave $
          merge0fa (slow 8 $ p2 4) $ merge0fa (p2 3) $
          appends [p1, p2 3, dense 2 p1])
    -- if I delete the "p2 3" from this last line,
    -- it takes effect immediately. (Using two instances of append
    -- instead of one cat has no effect.)
  ]
