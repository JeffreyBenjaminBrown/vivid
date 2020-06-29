-- module Montevideo.EarTrain.Convenience where

import EarTrain.EarTrain
import EarTrain.Audio
import EarTrain.Types


:l src/EarTrain/EarTrain.hs

earTrainChromatic 31 2 31
earTrainFromChordList 31 $ map (\n -> [0,n]) [24..29]

earTrainFromScale 46 2 [0,15,27] -- the major chord


playFreqs $ (*400) <$> [1,5/4+0.03,3/2-0.01]

earTrainFromChordList 31 $ map (\n -> [0,n]) [25..29]
earTrainFromChordList 31 $ map (\n -> [0,n]) [25,26]

earTrainFromChordList 31 $ map (\n -> [0,n]) [7,8]
