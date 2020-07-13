# How durations work

## The idea

Patterns in Montevideo are represented by the `Museq` type. 
For each `Museq` there are two associated durations.

* `dur` is the length of time the pattern plays when it is concatenated to other patterns.

* `sup` is the length of time it takes the pattern to play all the way through.

This is necessary because it might be that you want the `Museq` to do something different from one iteration to the next.


## An example

Suppose we create the following `Museq`s:

```
ks = mmh 2 -- this `Museq` has `dur` = `sup` = 2
  [ ("", 0, "kick")    -- it plays a kick at time 0
  , ("", 1, "snare") ] -- and a snare at time 1
hats = mmh 1 -- this `Museq` has `dur` = `sup` = 1
  [ ("", 0, "hat") ]  -- it plays a hat at time 0, and that's all
```

(A `Museq` can contain anything -- samples, synth isntructions, numbers, scales. In this case, for simplicity, they just contain strings. Each event is labeled with "" because we're not worrying about polyphony.)

If we concatenate those two patterns, we get what you would expect: a pattern of duration 3, with a kick on 0, a snare on 1, and a hat on 2:

```
> cat [ks,hats]
Museq { _dur = 3 % 1
      , _sup = 3 % 1
      , _vec = [ Event { _evLabel = ""
                       , _evArc = (0 % 1,1 % 1)
                       , _evData = "kick"}
               , Event {_evLabel = ""
                       , _evArc = (1 % 1,2 % 1)
                       , _evData = "snare"}
               , Event {_evLabel = ""
                       , _evArc = (2 % 1,3 % 1)
                       , _evData = "hat"}]}
```


Suppose we change the `dur` of ks to 1 instead of 2, 
by writing `ks0 = dur .~ 1 $ ks`. 
`ks0` still has the same `sup` (2) as `ks`.
If we play it by itself, it sounds unchanged.
But if we concatenate it with another pattern,
`ks0` will play for a duration of 1 instead of 2 before yielding to the other.

Here's what that looks like:
```
> cat [dur .~ 1 $ ks,hats]
Museq { _dur = 2 % 1,
        _sup = 4 % 1,
        _vec = [ Event { _evLabel = ""
                       , _evArc = (0 % 1,1 % 1)
                       , _evData = "kick"}
               , Event {_evLabel = ""
                       , _evArc = (1 % 1,2 % 1)
                       , _evData = "hat"}
               , Event { _evLabel = ""
                       , _evArc = (2 % 1,3 % 1)
                       , _evData = "snare"}
               , Event { _evLabel = ""
                       , _evArc = (3 % 1,4 % 1)
                       , _evData = "hat"}]}
```

Instead of "kick snare hat", we've got "kick hat snare hat".


# The duration-changing functions

`fast n m` makes the `Museq` named `m` faster. Its `dur` and sup, and the start and end times of its events, are all divided by `n`.

`slow n m` makes the `Museq` named `m` slower. Its `dur` and sup, and the start and end times of its events, are all multiplied by `n`.

`dense n m` leaves the `dur` of the pattern unchanged, but speeds it up, so that in the time it used to take to play once, it now plays `n` times. Without concatenation, `dense n` sounds exactly like `fast n`.

`sparse n m` leaves the `dur` of the pattern unchanged, but slows it down, so that in the time it used to take to play `n` times, it now plays only once. Without concatenation, `sparse n` sounds exactly like `slow n`.

`rep n` increases the `dur` of the pattern, and otherwise leaves it unchanged. Without concatenation, it sounds like it has had no effect, but with concatenation, the pattern will play `n` times instead of once before yielding to the other.

`rotate n` reduces the `dur` of the pattern, and otherwise leaves it unchanged. Without concatenation, it sounds like it has had no effect, but with concatenation, the pattern will play only `1/n` times instead of all the way through before yielding to the other.
