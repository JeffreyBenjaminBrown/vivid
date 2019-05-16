s2 = mmrt1 2 $ [ (0, "a")
               , (1, "b") ]

f1 = mmh 8 $ pre2 "a" $
  [ (0, id)
  , (7, \x -> stack x $ early (1/4) x) ]

meta f1 $ dense 4 $ s2
