-- Cycles between a note and silence, evenly spaced,
-- across the span of one second.

pat = ( mmh 1 $ pre2 "blark"
        [ (0, mfl [("freq", 600),
                   ("amp", 0.2),
                   ("on", 1)]),
          (1/2, m1 "on" 0) ] )

chAll $ mfl [
  ("1", nZot pat)
  -- ("2", nBoop pat)
  ]
