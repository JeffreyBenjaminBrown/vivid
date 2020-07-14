-- | PITFALL: Import this library qualified to avoid name conflicts.

module Montevideo.Monome.Config where


-- | * Configure the tuning

-- | Some (edo,spacing) pairs I like:
-- (31,6), (41,6), (46,7), (87,12 or 13)
edo, spacing, skip :: Num a => a
edo = 46  -- ^ Pick your temperament.

spacing = 9 -- ^ Pick the number of edo steps between one row
            -- and the next. Negative doesn't work yet.
skip = 1 -- ^ For the Kite Guitar tuning, set (edo,spacing,skip) = (41,13,2).
  -- For any other tuning (barring another Kite-ish breakthrough), set skip=1.
  -- TODO : Make the LEDs understand this.

octaveStretchInCents :: Double
octaveStretchInCents = 0 -- ^ Set to 0 for pure EDO (ED-2).
  -- For some useful recomendations re. stretch values, see
  -- http://x31eq.com/temper/net.html
  -- Particularly good (edo, stretch) values:
    -- 22 edo, -1.106 cents (TET-optimal in the 11-limit)
    -- 31 edo, 0.502 cents (TET-optimal in the 13-limit)


-- | * Configure the synth

-- | The amplitude of each voice.
amp :: Float
amp = 0.35

-- | The frequency of "note 0". (Notes can be negative.)
freq :: Num a => a
freq = 80
