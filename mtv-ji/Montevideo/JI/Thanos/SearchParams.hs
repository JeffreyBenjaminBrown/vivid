-- | To configure the search.

module Montevideo.JI.Thanos.SearchParams where


minEdo, maxEdo, maxModulus, max12edoFretSpan_lim5, max12edoFretSpan_lim7, max12edoFretSpan_lim13, alwaysConsiderAtLeastThisManyFrets :: Int
minEdo = 72 -- ^ Don't consider any edos smaller than this.
maxEdo = 72 -- ^ Don't consider any edos bigger than this.
maxModulus = 12
max12edoFretSpan_lim5 = 6 -- ^ Drop edos for which some 13-limit interval requires a stretch greater than this many frets of 12-edo.
max12edoFretSpan_lim7 = 8 -- ^ Drop edos for which some 13-limit interval requires a stretch greater than this many frets of 12-edo.
max12edoFretSpan_lim13 = 10 -- ^ Drop edos for which some 13-limit interval requires a stretch greater than this many frets of 12-edo.
alwaysConsiderAtLeastThisManyFrets = 5 -- ^ Even if there's a solution on the 0-fret (so probably the best), any solution on frets -10 to 10 will be considered.

isForGuitar :: Bool
isForGuitar = False -- ^ If searching for guitar, the ability to play a note on the same "string" as the root is excluded, because it means you can't play them both simultaneously.

minFretsPerOctave, maxFretsPerOctave, minSpacingIn12edo :: Float
minFretsPerOctave = 10 -- ^ when searching for guitars, this should probably be a big number. But for electronic instruments, big ones are fine -- maybe confusing, but certainly playable.
maxFretsPerOctave = 35
minSpacingIn12edo = 12/5
  -- ^ If this is 12/n, this considers only spacings that let you cover an octave in n+1 strings.
  -- For instance, if this is 3, consider only spacings
  -- between strings that are at least 3\12.
