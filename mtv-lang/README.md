# Quickstart to live-coding in `mtv-lang`

## Set up these dependencies

### Install [Stack](https://docs.haskellstack.org/en/stable/README/)

Stack is a Haskell build tool.

### Install SuperCollider

as described in the documentation for [Vivid](https://www.vivid-synth.com/).

### Install the Dirt-Samples library

Clone the
[Dirt-Samples](https://github.com/tidalcycles/Dirt-Samples) repository.

Edit `mtv-synth/Montevideo/Synth/Config.hs`, changing the definition of `dirtSamplesFolder` to point to the Dirt-Samples repo you cloned.


## Run it

* Go to the root folder of Montevideo.
* Run `./sc-start.sh` to start SuperCollider.
* Go to the `mtv-lang/` subfolder.
* Run`stack ghci`.
* Evaluate `:s init.hs`.
* Evaluate `:. sketches/2/3`.
* Evaluate `hush` to stop the music.
* Load another sketch with `:. sketches/something/else`.
* Turn it off with `off`. (This will leave SC running, so you can start another dispatcher by running `:s init.hs` again without restarting SC.)

The files in `docs/` are simple and commented.
The files in `sketches/`, not so much.

You could use other samples, too;
just edit that path to point to the folder your samples are in,
and then edit `src/Synths/Samples.hs` to point to each sample individually.


# Montevideo-lang vs. TidalCycles

This project is inspired by
[TidalCycles](https://tidalcycles.org/index.php/Welcome).
I love TidalCycles, but I wanted to express
some things that TidalCycles does not permit:

* Explicit control of the durations of patterns. I wanted, for instance,
to concatentate a 2-bar intro with a 16 bar verse and a 16 bar chorus,
and get a 34 bar pattern back.

* A way to send instructions to an ongoing note,
such as "bend up a halfstep".

* A convenient way to make scale patterns -- e.g. "E major! now F major!" --
in which the duration of each scale could be any real number,
independent of the other scales in the same pattern.

* An Applicative instance,
so you could make a pattern whose events were transformations,
and apply it to another pattern.

* Synths written in, and easily tweaked using, Haskell.
(Montevideo uses [Vivid](https://github.com/vivid-synth/vivid)
for this.)

Montevideo permits all that.
There are examples in the `docs/` folder.

TidalCycles has a better synth, an enormous library of functions,
extensive documentation, and a great community.

TidalCycles represents cycles as functions.
Montevideo represents cycles as data.
I still haven't remembered why I thought that was better.


# Learning it

The following might be a good order in which to study the example code.
(Each of these lines is something you can evaluate from GHCI,
assuming you ran `:s init.hs` as described above.)

```
:. docs/samples
:. docs/merge-two-patterns
:. docs/syn
:. docs/duration.md
:. docs/scale-progression
:. docs/scale-and-root-progression
:. docs/pattern-of-transformations
```

If its type signatures and comments aren't enough to understand a function,
check whether it's in the test suite (by grepping for it in mtv-test/).
