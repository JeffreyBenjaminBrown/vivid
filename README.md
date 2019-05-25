# Montevideo: a music toolkit

See the wiki for [what it's for|why it exists](https://github.com/JeffreyBenjaminBrown/montevideo/wiki/Why-Montevideo).


## prerequisites

To use this code you'll need to have first installed:

* [Stack](https://docs.haskellstack.org/en/stable/README/),
the Haskell toolkit

* SuperCollider,
as described in the documentation for [Vivid](https://www.vivid-synth.com/).


You'll also need to clone the [Dirt-Samples](https://github.com/tidalcycles/Dirt-Samples) repository, and then amend `src/Synths/Config.hs` to point to that repo. (You could use other samples, too; just edit that path to point to the folder your samples are in, and then edit `src/Synths/Samples.hs` to point to each sample individually.)


## to see how it works

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


## Super-experimental

To see how controlling `Montevideo` using
[Hode](https://github.com/JeffreyBenjaminBrown/hode) works,
try running `playSong disp testRslt 10`.


## Why the name

It's a mashup of monome, Tidal(Cycles) and Vivid.
(The [monome code](https://github.com/JeffreyBenjaminBrown/monome)
is elsewhere at the moment.)
