chars = mmh 2 $ pre2 "" $ [ (0, "a") ]

ops = mmh 2 $ pre2 "a" $
  [ (0, id)
  , (1, \x -> stack x x ) ]

-- (\x -> stack x x) $ dense 4 chars -- works
meta' ops $ dense 2 $ chars       -- doesn't work
