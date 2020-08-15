-- | To configure the search.

module Montevideo.JI.Thanos.SearchParams where


minEdo, maxEdo, maxModulus :: Int
minEdo = 30 -- ^ Don't consider any edos smaller than this.
maxEdo = 350 -- ^ Don't consider any edos bigger than this.
maxModulus = 17

minFretsPerOctave, maxFretsPerOctave, max12edoFretSpan, minSpacingIn12edo :: Float
minFretsPerOctave = 10
maxFretsPerOctave = 45
max12edoFretSpan = 7 -- ^ Drop edos for which some 13-limit interval requires a stretch greater than this many frets of 12-edo.
minSpacingIn12edo = 12/6
  -- ^ If this is 12/n, this considers only spacings that let you cover an octave in n+1 strings.
  -- For instance, if this is 3, consider only spacings
  -- between strings that are at least 3\12.
