# Vivid-Dispatch: like Tidal, using Vivid and Hode


## prerequisites
To use this code you'll need to have first installed:

* [Stack](https://docs.haskellstack.org/en/stable/README/),
the Haskell toolkit

* SuperCollider,
as described in the documentation for [Vivid](https://www.vivid-synth.com/).


## to see how it works

Run `bash sc-start.sh` from the command line to start SuperCollider
(listening on an appropriate channel).

Run `stack ghci` to start the Haskell repl.
Run `:s init.hs` to set up a dispatcher
(a thing that talks to SuperCollider).

To see how controlling `Vivid-Dispatch` from code works,
try running `:. demo/x`
(where `x` is the name of some file in the `demo/` folder).

To see how controlling `Vivid-Dispatch` using
[Hode](https://github.com/JeffreyBenjaminBrown/hode) works,
try running `playSong disp testRslt 10`.
