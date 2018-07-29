:set -XDataKinds
s <- synth vap ()


-- | just sin fm
set s (400 :: I "freq")
set s (400 :: I "fm-amp")
set s (750 :: I "fm-freq")

set s (100 :: I "freq")
set s (400 :: I "fm-amp")
set s (750 :: I "fm-freq")

set s (300 :: I "freq")
set s (300 :: I "fm-amp")
set s (66.66 :: I "fm-freq")

set s (300 :: I "freq")
set s (350 :: I "fm-amp")
set s (66.66 :: I "fm-freq")


-- | just sine fm with the fm scaled by noise

set s (300 :: I "freq")
set s (500 :: I "fm2-amp")
set s (100 :: I "fm2-freq")
set s (500 :: I "nz-lpf")

set s (300 :: I "freq")
set s (5000 :: I "fm2-amp")
set s (100 :: I "fm2-freq")
set s (500 :: I "nz-lpf")

set s (300 :: I "freq")
set s (5000 :: I "fm2-amp")
set s (100 :: I "fm2-freq")
set s (5 :: I "nz-lpf")

-- warble, beautiful
set s (300 :: I "freq")
set s (10000 :: I "fm2-amp")
set s (20 :: I "fm2-freq")
set s (5 :: I "nz-lpf")

set s (300 :: I "freq")
set s (3000 :: I "fm2-amp")
set s (300 :: I "fm2-freq")
set s (50 :: I "nz-lpf")

-- | now with some saw too
set s (1 :: I "saw")
set s (300 :: I "freq")
set s (500 :: I "fm2-amp")
set s (100 :: I "fm2-freq")
set s (500 :: I "nz-lpf")

set s (1 :: I "saw")
set s (300 :: I "freq")
set s (5000 :: I "fm2-amp")
set s (100 :: I "fm2-freq")
set s (500 :: I "nz-lpf")

set s (1 :: I "saw")
set s (300 :: I "freq")
set s (5000 :: I "fm2-amp")
set s (100 :: I "fm2-freq")
set s (5 :: I "nz-lpf")

-- ribbony, beautiful
set s (1 :: I "saw")
set s (300 :: I "freq")
set s (10000 :: I "fm2-amp")
set s (20 :: I "fm2-freq")
set s (5 :: I "nz-lpf")

set s (0.5 :: I "saw")
set s (300 :: I "freq")
set s (3000 :: I "fm2-amp")
set s (300 :: I "fm2-freq")
set s (50 :: I "nz-lpf")

-- | with feedback
set s (0.5 :: I "saw")
set s (300 :: I "freq")
set s (3000 :: I "fm2-amp")
set s (300 :: I "fm2-freq")
set s (50 :: I "nz-lpf")
set s (0.4 :: I "delay-amp")
set s (0.3 :: I "delay-freq")
