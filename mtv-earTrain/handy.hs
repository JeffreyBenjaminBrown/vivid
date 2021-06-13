-- Roughly 24-edo, in 46-edo.
someIntervals = let ps = [ 2, 4, 6, 8, 10,12,
                           15,17,19,21,23,25,
                           27,29,31,32,
                           37,39,42,44,
  in ps ++ (23 : map (\p -> 46-p) ps)


earTrainFromChordList 46 $ map (\x -> [0,x]) $ someIntervals

earTrainFromChordList 46 $ map (\x -> [0,x]) $ [31,32]

earTrainFromChordList 46 $ map (\x -> [0,x,27]) [12..15]
earTrainFromChordList 46 $ map (\x -> [0,x,27]) [10,12]
earTrainFromChordList 46 $ map (\x -> [0,x,27]) [10..13]
earTrainFromChordList 46 $ map (\x -> [0,x,27]) [17,18]

earTrainFromChordList 46 $ map (\x -> [0,x]) [37,39]
earTrainFromChordList 46 $ map (\x -> [0,x,27]) [12..15]
earTrainFromChordList 46 $ map (\x -> [0,x,27]) [15..19]
earTrainFromChordList 46 $ map (\x -> [0,x,27]) [8..12]

earTrainFromChordList 46 $ map (\x -> [0,x]) [26,28]

earTrain3ClusterFreeChromatic 46 3 150

earTrain3ClusterFreeChromatic 22 3 22

let base = [10,12]
    mirrored = base ++ map (22 -) base
    dyads = S.toList . S.fromList $
            map (\x -> [0,x]) mirrored
  in earTrainFromChordList 22 dyads
