# The best EDOs

I keep computing this and staring at it:

```
let ws = [3,4,5,6,7]
myPrint $ filter ((< 290) . snd) $ [(n, errorSum ws n) | n <- [1..60]]
myPrint $ filter ((< 160) . snd) $ [(n, errorSum ws n) | n <- [60..100]]
myPrint $ filter ((< 90)  . snd) $ [(n, errorSum ws n) | n <- [100..150]]
myPrint $ filter ((< 60)  . snd) $ [(n, errorSum ws n) | n <- [150..200]]
myPrint $ filter ((< 50)  . snd) $ [(n, errorSum ws n) | n <- [200..250]]
myPrint $ filter ((< 40)  . snd) $ [(n, errorSum ws n) | n <- [250..300]]
```


# 37 edo

I couldn't sleep last night, was thinking about 37-edo. Composing without perfect fifths seems like a useful harmonic exercise. It's never occurred to me before but I wonder whether I use that interval as a kind of crutch. It's almost always present in my music, and my deviations from it are very 12-et: basically aug, dim and dom7 b5.

Leaving out factors of 3, 37-edo will give me all the intervals I want, in exactly one form:

wide min 2nd (14:13)
neutral 2nd (11:10)
wide maj 2nd (8:7)
min 3rd (13:11)
neutral 3rd (16:13)
harmonic major third (5:4)
wide major third (14:11)
super-narrow fourth (13:10, 454 cents)
super-wide fourth (11:8)
the septimal tritone

I tried it. Playing that way is hard.


# 41, 46, 53 edo: great

# 72 edo

After 53, the next standout according to `errorSum` is 72.
In 72-edo, `compareGrids` shows that a column spacing of 8\72 works well -- 
as close as possible to a square, and the alignments are all in [-2,2].

10\72 is also nice -- even nicer than it looks,
because the octave falls in row 2, so something in row 2 is not just 2 rows away from the origin but 0 rows from the octave.


# 87 is astounding

By many reasonable errorSum arguments, 72 come 77 and 84, and then 87 obliterates everything before it, and everything else until 130 (or until 118 if you care suffiently more strongly about the lower primes).

The best ways to arrange 87 look like they are with 10\87 or 12\87 between columns. 12\87 gives a wider vertical spread of the harmonics, but it puts an octavge in only 7 columns, which is nice for 7-tone scales.


# None of the standouts beyond 87 stand out like 87 does

After that, the standouts are:

```
maybe 118
130
maybe 140
maybe 183
224
270
```

But 87 has a total error of only 6.2 cents on the first 5 odd primes. 224, with almost 3 times as many notes, only gets that down to 2.6 cents. By 270 it's at 1.4.

By contast, consider the bargain in going from 53-edo (the edo below 87 with the least unweighted sum) to 87-edo:

```
> 87/53
1.6415094339622642
> errorSum [1,1,1,1,1] 53
169
> errorSum [1,1,1,1,1] 87
62
> fromIntegral  (errorSum [1,1,1,1,1] 53) / fromIntegral (errorSum [1,1,1,1,1] 87)
2.725806451612903
```

That is, by raising the number of notes by less than two-thirdsfrom the previous standout, the error sum falls by almost two-thirds.

Following the rule that an n% increase in the number of notes should only be undertaken if it is accompanied by a roughly similar decrease in the error sum, one would jump, uh ... to 19, from 19 to 22, from 22 to 26, then 31, 37, 53, 87, 224, 270 ... gee maybe that series just keeps going. I expected it to end.
