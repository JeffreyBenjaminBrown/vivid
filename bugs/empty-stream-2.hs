-- | Like empty-stream.hs, but for use with `meta''`.

chars = mmh 2 $ pre2 "" $ [ (0, "a") ]

ops = mmh 2 $ pre2 "" $
  [ (0, ("id",id))
  , (1, ("\\x -> stack x x", \x -> stack x x) ) ]

-- (\x -> stack x x) $ dense 2 chars -- works
m = meta'' ops $ dense 2 $ chars       -- doesn't work
