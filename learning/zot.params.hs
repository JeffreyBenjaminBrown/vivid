s <- synth zot ()
set s (0.1 :: I "amp", (1/300) :: I "del", 0.5 :: I "pulse")
set s               (1 :: I "pm-b", 1 :: I "pm-m", 1 :: I "pm-f")
set s (400 :: I "f", 3 :: I "fm-b", 30 :: I "fm-m", 300 :: I "fm-f")
set s (0.5 :: I "w", 1 :: I "wm-b", 30 :: I "wm-m", 300 :: I "wm-f")
set s (0.1 :: I"am",0.5::I"am-b",10::I"am-f")

t <- synth zot ()
set t (0.1 :: I "amp", (1/300) :: I "del", 0.5 :: I "pulse")
set t               (1 :: I "pm-b", 1 :: I "pm-m", 1 :: I "pm-f")
set t (400 :: I "f", 3 :: I "fm-b", 30 :: I "fm-m", 300 :: I "fm-f")
set t (0.5 :: I "w", 1 :: I "wm-b", 30 :: I "wm-m", 300 :: I "wm-f")

