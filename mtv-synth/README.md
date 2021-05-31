Parameters for these synths can be confusing.
Most synths have an "amp" parameter.
Some can also be controlled by an "on" parameter.
Hopefully eventually all of them will be.
The preferred way to control *whether* a voice is sounding is via "on",
whereas "amp" should be reserved for controlling its level.
Don't send "amp=0" messages, because they erase the amplitude state.

For instance, this is good form:
```haskell
pat = ( mmh 1 $ pre2 "blark"
        [ (0, mfl [("freq", 600),
                   ("amp", 0.2),
                   ("on", 1)]),
          (1/2, m1 "on" 0) ] )

chAll $ mfl [
  ("1", nZot pat)
  -- ("2", nBoop pat)
  ]
```

The `onOffEnvelope` SynthDef uses the "on" parameter (and some others).
The synths that respond to "on" messages are exactly those that use it.
