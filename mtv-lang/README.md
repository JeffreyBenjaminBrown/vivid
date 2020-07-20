# Quickstart to live-coding in `Montevideo-lang`

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

See the section on "learning", below, for where to go next.

## You can also add your own samples

Just edit that path to point to the folder your samples are in,
and then tediously edit `src/Synths/Samples.hs`
to reify each new sample.


# Montevideo-lang is inspired by [TidalCycles](https://tidalcycles.org/index.php/Welcome)

I love TidalCycles. It has a friendly DSL,
a fancy, delicious sampler,
a giant library of useful functions,
extensive documentation, and a wonderful community.

Montevideo does not --
but it offers expressive capabilities absent from TidalCycles:

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

Montevideo also has a freaky synth called Zot,
based on Reaktor Spark.

TidalCycles represents cycles as functions.
Montevideo represents cycles as data.
I still haven't rediscovered why I thought that would be better.


# Learning Montevideo-lang

## Learning to use it

The files in `docs/` are simple and commented, and a good way to learn.
This might be a good order to learn them in.
(Each of the following lines is an instruction you can evaluate from GHCI,
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

The files in `sketches/` are neither simple nor well commented.


## Known problems

### Synths drop notes at high frequencies

The sampler (which is the way to create drum loops)
starts dropping notes at around 20 Hz,
i.e. when there's only 50 ms or less between consecutive notes.

The other synths, which are not sample-based,
start dropping notes at around 52 Hz.
(Note that the parameters of your synths can change more frequently than that,
but you've got to program that into them using Vivid,
rather than simply sending messages to them from Montevideo-lang.)

In both cases, when you push that envelope,
it still sounds good, but it's not perfectly regular.


### Each sample needs to play in a separate voice, I think

My experiments suggest it's true, but I don't know why.

For instance, if a beat sends events to the `Sampler SampleKr` synth
and to the `Sampler SampleSm_m` synth,
the events sending to the first should have an `_evLabel`
field different from the events sending to the other.


## Hacking it

If its type signature and comments aren't enough to understand a function,
check whether it's in the test suite
(by grepping for it in the mtv-test/ folder).
