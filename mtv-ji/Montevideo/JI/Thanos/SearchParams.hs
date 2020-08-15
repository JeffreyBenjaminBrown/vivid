-- | To configure the search.

module Montevideo.JI.Thanos.SearchParams where


minEdo, maxEdo, maxModulus, max12edoFretSpan_lim5, max12edoFretSpan_lim7, max12edoFretSpan_lim13
  :: Int
minEdo = 30 -- ^ Don't consider any edos smaller than this.
maxEdo = 350 -- ^ Don't consider any edos bigger than this.
maxModulus = 17
max12edoFretSpan_lim5 = 6 -- ^ Drop edos for which some 13-limit interval requires a stretch greater than this many frets of 12-edo.
max12edoFretSpan_lim7 = 8 -- ^ Drop edos for which some 13-limit interval requires a stretch greater than this many frets of 12-edo.
max12edoFretSpan_lim13 = 10 -- ^ Drop edos for which some 13-limit interval requires a stretch greater than this many frets of 12-edo.

minFretsPerOctave, maxFretsPerOctave, minSpacingIn12edo :: Float
minFretsPerOctave = 5
maxFretsPerOctave = 30
minSpacingIn12edo = 12/5
  -- ^ If this is 12/n, this considers only spacings that let you cover an octave in n+1 strings.
  -- For instance, if this is 3, consider only spacings
  -- between strings that are at least 3\12.
