-- Montevideo lets you change the sound of a synthesizer
-- in the middle of a note. The pattern below is not a series
-- of distinct notes; it is a single synth voice, playing continuously,
-- and continuously receiving instructions to change
-- frequency or other parameters.

patMelody = -- A melody (in Hz).
  mmho 4    -- This `Museq`'s duration will be 4.
    -- "mmho" = "mm" (make a Museq)
    --        + "h" (hold each note until the next one)
    --        + "o" (insert "on=1" messages wherever "on" is not mentioned)
  $ pre2 "" -- This pattern is not polyphonic, so each event can be
            -- given the same label  -- in this case, the empty string.
  [ (0                    -- At time 0,
    , mfl [("freq",140)]) -- map the "freq' parameter to 140.
  , (1,   mfl [("freq",180)])
  , (2,   mfl [("freq",240)])
  -- The next two events last half as long as the previous three.
  , (3,   mfl [("on"  ,0 )])  -- "on=0" means "note off"
  , (3.5, mfl [("freq",560)])
  ]

-- Another melody. Since in Hz they would be below the human range of hearing,
-- these frequencies are most naturally thought of as
-- relative to the Hz values in the other one. For three quarters of this
-- loop (from time 0 to time 3/2), the frequency is 1, i.e. unchanged.
-- For the last quarter (from time 3/2 to time 2) the frequency is 8,
-- i.e. 3 octaves higher.
patOctave = mmho 2 $ pre2 ""
  [ (0,   mfl [("freq",1)])
  , (3/2, mfl [("freq",8)]) ]

-- A "timbre melody". It refers to parameters defined for the Zot synth.
-- It's not really important what these parameters mean;
-- the point is that each parameter can be updated while the synth plays.
patPulse = mmh 2 $ pre2 ""
  [ (0, mfl [ ("pulse",1)   -- The carrier is entirely a pulse wave.
            , ("amp", 0.02) -- Amplitude.
            ])
  , (1, mfl [ ("pulse",0)   -- The carrier is entirely a sine wave.
            , ("amp", 0.04)
            ])
  ]

-- Another "timbre melody", involving yet more Zot parameters.
patFm = mmh 2 $ pre2 ""    -- This pattern also has a duration of 2.
  [ ( 0                    -- At time 0,
    , mfl [ ("fm-m",0) ] ) -- turn off the frequency modulation.
  , ( 1                    -- At time 1,
    , mfl [ ("fm-f",1/4)   -- The frequency of the FM signal is 1/4 the
                           -- frequency of the carrier signal.
          , ("fm-m",2/3)   -- The amplitude of the FM signal is 2/3 the
          ] ) ]            -- frequency of the carrier signal.

-- If this is confusing, see `docs/merge-two-patterns.hs"
-- for a simpler example.
--
-- The `merge1` functions below cause like parameters to be multiplied.
-- Most of the patterns do not deal in the same parameters,
-- so there's no multiplication, just juxtaposition.
-- However, patOctave and patMelody both control `freq`,
-- so `merge1` causes their values to be multiplied.
chAll $ mfl
  [ ( "1"
    , nZot $ -- Send the following `Museq` to the `Zot` synth.
      merge1 (fast 3 patPulse) $
      merge1 (fast 2 patFm) $
      merge1 (fast 4 patOctave)
      patMelody
    ) ]
