s <- synth zot ()
set s (0.1 :: I "amp", (1/300) :: I "del", 0.5 :: I "pulse", 400 :: I "f")

set s (2 :: I"sh", 2::I"sh-b")

set s (2::I"sh",0.2::I"sh-b")
set s (1 :: I"rm",2::I"rm-b",0::I"rm-f")


set s               (1 :: I "pm-b", 1 :: I "pm-m", 1 :: I "pm-f")
set s               (44 :: I "fm-b", 30 :: I "fm-m", 300 :: I "fm-f")
set s (0.5 :: I "w", 1 :: I "wm-b", 30 :: I "wm-m", 300 :: I "wm-f")
set s (0.7 :: I"rm",0.25::I"rm-b",100::I"rm-f")
set s (0.1 :: I"am",0.5::I"am-b",10::I"am-f")
set s (3000 :: I"bpf",1 :: I"bpf-m",0.5::I"bpf-q")
set s (500 :: I"hpf",1 :: I"hpf-m")
set s (2000 :: I"lpf",1 :: I"lpf-m")
set s (2 :: I"lim")

t <- synth zot ()
set t (0.1 :: I "amp", (1/300) :: I "del", 0.5 :: I "pulse", 400::I"f")
set t               (1 :: I "pm-b", 1 :: I "pm-m", 1 :: I "pm-f")
set t (400 :: I "f", 3 :: I "fm-b", 30 :: I "fm-m", 300 :: I "fm-f")
set t (0.5 :: I "w", 1 :: I "wm-b", 30 :: I "wm-m", 300 :: I "wm-f")

