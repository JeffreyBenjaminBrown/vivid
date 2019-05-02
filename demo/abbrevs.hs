p f g = mmho 3 $ -- mmho = make museq + holds + ons
                 -- 'h': hold each event until the next
                 -- 'o': insert "on=1" wherever "on" isn't 0
  pre2 "a" -- to each of these pairs, prefix "a"
  [ (0, m1 "freq" f)
  , (1, m1 "freq" $ f*g)
  , (2, m1 "on" 0) ]

m = mmh 9 -- mmh = make museq' + holds
-- (ons only makes sense for Msg payloads; these are functions)
  [ trip "a" 0 id
  , trip "a" 3 $ fast 2
  , trip "b" 3 $ fast 4
  -- Wart: For safety, to prevent hanging notes,
  -- all voices must be turned off explicitly.
  , trip "a" 7 $ offs
  , trip "b" 7 $ offs
  ]

m2 = mmh 9 $ pre2 "a" [ (0,id), (5,early (1/2)) ]

chAll $ mfl
  [ ("1", nBoop $ -- nBoop :: Museq l Msg -> Museq l Note
          meta m $
          merge0a (p 400 $ 5/4) $
          meta m2 $ fast 2 $ p 1 $ 3/5 )
  , ("2", nBoop $
          meta (fast 2 m) $
          merge0a (p 300 $ 10/11) $
          fast 2 $ p 1 $ 3/7 ) ]

ch "3" $ nBoop $ -- ch = change
  meta m $
  merge0a (p 500 $ 10/11) $
  fast 2 $ p 1 $ 13/9
