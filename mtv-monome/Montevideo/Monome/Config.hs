-- | PITFALL: Import this library qualified to avoid name conflicts.

module Montevideo.Monome.Config where


-- | * Configure the tuning

-- | Change these values to tune the synth.
-- Read about `EdoConfig`, below, to see what the numbers mean.
theConfig = EdoConfig
  { _edo = 46
  , _spacing = 6
  , _skip = 1
  , _octaveStretchInCents = 0
  }

data EdoConfig = EdoConfig
  { _edo :: Int -- ^ The EDO. For most music, this value is 12.
  , _spacing :: Int -- ^ The distance in steps of the EDO between columns
                    -- of buttons on the monome. Positive numbers only.
                    -- For a guitar-like layout, edo=12 and spacing=5.
                    -- Some other (edo,spacing) pairs I like:
                    -- (31,6), (41,6), (46,7), (87,12 or 13)
  , _skip :: Int -- ^ You'll probably want this to be 1, in which case
    -- each button in a column is one EDO-step lower than the one below it.
    -- For the Kite Guitar tuning, set (edo,spacing,skip) = (41,13,2).
    -- PITFALL | TODO : The LED pattern that lights up when you play
    -- makes no sense if skip is anything but 1.
  , _octaveStretchInCents :: Double  -- ^ Set to 0 for pure EDO (ED-2).
    -- For some useful recomendations re. stretch values, see
    -- http://x31eq.com/temper/net.html
    -- Some particularly good (edo, stretch) values:
      -- 22 edo, -1.106 cents (TET-optimal in the 11-limit)
      -- 31 edo, 0.502 cents (TET-optimal in the 13-limit)
  } deriving (Show, Eq, Ord)

-- | DEPRECATED: These should go away.
edo                  =  _edo                  theConfig
spacing              =  _spacing              theConfig
skip                 =  _skip                 theConfig
octaveStretchInCents =  _octaveStretchInCents theConfig


-- | * Configure the synth

-- | The amplitude of each voice.
amp :: Float
amp = 0.35

-- | The frequency of "note 0". (Notes can be negative.)
freq :: Num a => a
freq = 80
