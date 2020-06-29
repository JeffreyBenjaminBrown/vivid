{-# LANGUAGE ScopedTypeVariables
#-}

module Montevideo.EarTrain.Types where


type PlayQuestion = IO () -- ^ make a sound, for the user to identify
type ShowAnswer = IO () -- ^ display something, e.g. "it was a major chord"
type Test = (PlayQuestion, ShowAnswer)

type Edo = Int -- ^ an equal division of the octave:
  -- 12 for normal music, 31 for exceptionally awesome music,
  -- 53 for music with perfect approximations to 3:2 and 5:4, etc.
