-- Montevideo lets you change the sound of a synthesizer
-- in the middle of a note. The pattern below is not a series
-- of distinct notes; it is one voice of one synth, playing continuously,
-- and continuously receiving instructions to change
-- frequency or other parameters.

patFreq = -- a melody (in Hz)
  mmho 4  -- The duration will be 4.
  -- "mmho" = "mm" (make a Museq)
  --           + "h" (hold each note until the next one)
  --           + "o" (insert "on=1" messages whereer "on" is not mentioned)
  $ pre2 ""
  [ (0,   mfl [("freq",70)])
  , (1,   mfl [("freq",90)])
  , (2,   mfl [("freq",60)])
  , (3,   mfl [("on"  ,0 )])      -- "on=0" means "note off"
  , (3.5, mfl [("freq",140)])
  ]

-- A "timbre melody". It refers to parameters defined for the Zot synth.
-- It's not really important what these parameters mean;
-- the point is that each parameter can be updated while the synth plays.
patTimbre = fast 2 $ mmh 2 $ pre2 ""
  [ (0, mfl [ ("pulse",1)   -- the carrier is entirely a pulse wave
            , ("amp", 0.02) -- amplitude
            , ("fm-b",0)    -- feedback from the fm signal
            ])
  , (1, mfl [ ("pulse",0)   -- the carrier is entirely sine wave (no pulse)
            , ("amp", 0.04)
            , ("fm-f",1/3)  -- the frequency of the fm signal is 1/3 the
                            -- frequency of the carrier signal
            , ("fm-m",1/10) -- the amplitude of the fm signal is 1/10 the
                            -- frequency of the carrier signal
            , ("fm-b",5)    -- feedback from the fm signal
            ])
  ]

chAll $ mfl
  [ ("1", nZot $
          merge1 patFreq patTimbre) ]
