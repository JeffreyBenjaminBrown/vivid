octaveHunt edo octave stringGap fretGap = ( let
  octaveInSteps = edo * octave
  in filter ((== 0) . flip mod' 1 . snd) $
     map (\string ->
            ( string,
              (octaveInSteps - string*stringGap)
              / fretGap))
    [-10 .. 10] )
myPrint $ octaveHunt 65 1 11 8

primes15no13 = filter (/= (13%8)) $ primesOctave1 15

ls = bestEdoLayouts (primesOctave1 13) [48..140]

lastVisited = last $ take 30 $ drop 30 $ filter okayHarmony $ drop 120 ls

-- Using this I searched the edos [50..140]
-- past the 4x8s and into the 6x6s,
-- excluding anything with stringWidth > 7,
-- adding 2 to stringWidth for the area computation,
-- letting the two gaps vary anywhere from 1 to
-- round (e * (5/12) ).
okayHarmony = not . flip elem [
    40, 42, 45, 47, 49, 51, 52, 54, 59, 61, 66, 71
  ] . (^. _1) . unTuning . etrTuning
ls = bestEdoLayouts (primesOctave1 15) [24]
Pr.pPrint (
  reverse
  $ take 30
  $ zip [1..]
  $ L.sortBy (comparing $ etrArea)
  $ filter okayHarmony
  $ filter ((\x -> 0.3 <= x && x <= 0.7) . (^. _4) . unArea . etrArea)
  $ ls )

-- ditching 21 and 17, and using the 2nd best 11:8
notes = [1 % 1,2 % 1,3 % 2,5 % 4,7 % 4,9 % 8,15/11,13 % 8,15 % 8,19 % 16,23 % 16,25 % 16,27 % 16,29 % 16,31 % 16]
Pr.pPrint $ bestLayout (60,19,3) (notes ) & _2 %~ map LayoutRow'

135: 6*(4**2) + 8**2
94: sum $ map (**2) [4,1,5,3,1,4]

-- A single tuning's layouts in some limit.
( Pr.pPrint
  $ reverse
  $ take 20
  $ map (_1 %~ Tuning')
  $ map (_3 %~ map LayoutRow')
  $ L.sortBy (comparing (^. _2) )
  $ tuningAreaLayouts 41 (primesOctave1 13) )

-- *maybe* I read the first 120 of these
-- but I think I flubbed some of the last 60
-- by reversing halfway through.
-- For these ls was from 40 .. 210
Pr.pPrint ( take 30 $ drop 90
  $ L.sortBy (comparing $ etrArea . snd)
  $ reverse $ L.sortBy (comparing $ etrEdo . snd)
  $ zip [1..]
  $ filter okayHarmony ls )

let
  lim31_sharp11 = [1 % 1,2 % 1,3 % 2,5 % 4,7 % 4,9 % 8,18/13,13 % 8,15 % 8,17 % 16,19 % 16,21 % 16,23 % 16,25 % 16,27 % 16,29 % 16,31 % 16]
  in Pr.pPrint $ bestLayout (75,12,5) lim31_sharp11 & _2 %~ map LayoutRow'
