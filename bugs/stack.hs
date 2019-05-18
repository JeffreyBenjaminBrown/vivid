c1 = sup .~ 1 $ mmh 2 $ pre2 "" $ [ (0, "a") ]
  -- maybe this should be an invalid Museq
stack c1 c1

c2 = dur .~ 2 $ mmh 1 $ pre2 "" $ [ (0, "a") ]
  -- but this is certainly valid, and it fails too
stack c2 c2

c3 = mmh 1 $ pre2 "" $ [ (0, "a") ]
stack c3 c3
