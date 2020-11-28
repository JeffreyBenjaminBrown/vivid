{-# LANGUAGE ScopedTypeVariables
#-}

module Montevideo.EarTrain.Types where

type PlayQuestion = IO () -- ^ make a sound, for the user to identify
type ShowAnswer = IO () -- ^ display something, e.g. "it was a major chord"
type Test = (PlayQuestion, ShowAnswer)
