-- Cycles between a note and its octave, evenly spaced,
-- across the span of one second.

m = Museq
    { _dur = 1
    , _sup = 1
    , _vec = V.fromList
             [ Event { _evLabel = "a"
                     , _evArc = (0, 1/2)
                     , _evData = Note
                                 { _noteSd = Zot
                                 , _noteScParams = M.fromList
                                   [ ("freq", 440) ]
                                 } }
             , Event { _evLabel = "a"
                     , _evArc = (1/2,1)
                     , _evData = Note
                                 { _noteSd = Zot
                                 , _noteScParams = M.fromList
                                   [ ("freq", 220) ]
                                 } }
             ] }

chAll $ mfl [
    ("1", m )
    ]
