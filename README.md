# Montevideo: a toolkit for live-coding music in Haskell


## what it's for

Montevideo is inspired by
[TidalCycles](https://tidalcycles.org/index.php/Welcome).
I love TidalCycles, but I wanted to express
some things that TidalCycles does not permit:

* Explicit control of the durations of patterns. I wanted, for instance,
to concatentate a 2-bar intro with a 16 bar verse and a 16 bar chorus,
and get a 34 bar pattern back.

* A way to send instructions to an ongoing note,
such as "bend up a halfstep".

* A convenient way to make scale patterns,
in which the duration of each scale could be any real number,
independent of the other scales in the same pattern.

* An Applicative instance,
so you could make a pattern whose events were transformations,
and apply it to another pattern.

* Synths written in, and easily tweaked using, Haskell.
(Montevideo uses [Vivid](https://github.com/vivid-synth/vivid)
for this.)

Montevideo permits aall that.
There are examples in the `docs/` folder.


## setting it up

To use this code you'll need to have first installed:

* [Stack](https://docs.haskellstack.org/en/stable/README/),
the Haskell toolkit

* SuperCollider,
as described in the documentation for [Vivid](https://www.vivid-synth.com/).

You'll also need to clone the [Dirt-Samples](https://github.com/tidalcycles/Dirt-Samples) repository, and then amend `src/Synths/Config.hs` to point to that repo. (You could use other samples, too; just edit that path to point to the folder your samples are in, and then edit `src/Synths/Samples.hs` to point to each sample individually.)


## using it

Run `bash sc-start.sh` from the command line to start SuperCollider
(listening on an appropriate channel).

Run `stack ghci` to start the Haskell repl.
Run `:s init.hs` to set up a dispatcher
(a thing that talks to SuperCollider).

To see how controlling `Montevideo` from code works,
try running `:. docs/x` or `sketches/x`
(where `x` is the name of some file in the `demo/` folder,
minus the `.hs` extension).
The files in `docs/` are simple and commented;
the files in `sketches/`, not so much.


## why the name

It's a mashup of monome, Tidal(Cycles) and Vivid.
(The [monome code](https://github.com/JeffreyBenjaminBrown/monome)
is elsewhere at the moment.)
