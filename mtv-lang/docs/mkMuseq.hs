-- | "With great freedom comes great complexity." -- Jeff
--
-- There are lots of ways to make a Museq.
-- To explore those ways,
-- evaluate each paragraph of this interactively.

-- | You'd expect this to have gaps from time 1 to 2, but it doesn't,
-- because it's missing the "off" messages that would silence that part.
fe = ( fast 3 $ mkMuseq 3
       [ ("a", RTime 0, RTime 1, M.fromList [ ( "on", 1 )
                                            , ( "freq", 440 ) ] )
       , ("a", RTime 2, RTime 3, M.fromList [ ( "on", 2 )
                                            , ( "freq", 550 ) ] ) ] )
chAll $ mfl [ ("fe", nZot fe) ]

-- | `insertOffs` is the natural solution.
chAll $ mfl [ ("fe", nZot $ insertOffs fe) ]

-- | You might think `mkMuseq_holdMaybe` provides another solution.
-- But it in fact doesn't insert offs either;
-- it just holds each note until the next thing in the list.
-- A Nothing doesn't stop the parameters preceding it.
--
-- `mkMuseq_holdMaybe` is good for legato passages with silences.
-- However, in the particular case that the payload is `ScParams`,
-- without using `insertOffs` too, it's essentially the same as `mkMuseqHold`.
hm = ( fast 4 $ mkMuseq_holdMaybe 4
       [ ("a", RTime 0, Just $ M.fromList [ ( "on", 1 )
                                          , ( "freq", 440 ) ] )
       , ("a", RTime 2, Just $ M.fromList [ ( "on", 2 )
                                          , ( "freq", 550 ) ] )
       , ("a", RTime 3, Nothing ) ] )
chAll $ mfl [ ("fe", nZot hm) ]

-- | This lets us hear the Nothing in `hm`.
chAll $ mfl [ ("fe", nZot $ insertOffs hm) ]

-- | The ("on",1) passages in the definitions of `fe` and `hm` feel verbose.
-- Thanks to `insertOns`, they are unneeded.
oohm = ( fast 4 $ mkMuseq_holdMaybe 4
       [ ("a", RTime 0, Just $ M.fromList [ ( "freq", 440 ) ] )
       , ("a", RTime 2, Just $ M.fromList [ ( "freq", 550 ) ] )
       , ("a", RTime 3, Nothing ) ] )
chAll $ mfl [ ("fe", nZot $ insertOffs $ insertOns oohm) ]

-- | Happily, the order of `insertOns` and `insertOffs` doesn't matter,
-- because `insertOns` only makes changes where
-- the `on` parameter is not already defined.
-- (`ioio` is a shortcut for the more efficient of the two orders.)
chAll $ mfl [ ("fe", nZot $ insertOns $ insertOffs oohm) ]

-- | However, if you `insertOffs` without `insertOns`,
-- you'll have no ons, and the passage will sound at most once:
chAll $ mfl [ ("fe", nZot $ insertOffs oohm) ]

-- | If you `insertOns` without `insertOffs`, the result is more useful,
-- but you'll have no offs:
chAll $ mfl [ ("fe", nZot $ insertOns oohm) ]

pat = mmho dur0 $ pre2 "a"
  [ (0, m1 "freq" 0)
  , (1/2, m1 "freq" 1)
  , (1, m1 "freq" 2)
  , (2, m1 "freq" 3)
  , (3, m1 "freq" 4)
  , (4, m1 "freq" 5)
  , (5, m1 "on" 0) ]

scalePat = mmh (4*dur0) $ pre2 "a"
  [ ( 0     , maj3 )
  , ( dur0  , dim  )
  , ( 2*dur0, aol3 )
  , ( 3*dur0, aug  ) ]

toScale = nBoop
          . ops [("freq", (*) 300 . \p -> 2**(p/12))]
          . scale 12 scalePat

chAll $ mfl [
    ("1", toScale $ ops [("freq",((-) 12))] $ rev pat)
  , ("2", toScale $ ops [("freq",(+ 2))] $ fast 2 pat)
  , ("3", toScale $ ops [("freq",(+ 4))] $ fast 4 $ early 2 $ pat) ]
