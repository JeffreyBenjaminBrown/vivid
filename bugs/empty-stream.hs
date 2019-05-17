chars = mmh 2 $ pre2 "a" $ [ (0, "a")
                           , (1, "b") ]

ops = mmh 8 $ pre2 "a" $
  [ (0, id)
  , (7, \x -> stack x x ) ]

(\x -> stack x x) $ dense 4 chars -- works
meta' ops $ dense 4 $ chars        -- doesn't work
