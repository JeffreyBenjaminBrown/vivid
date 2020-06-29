Back when this software was limited to 31-edo, I demonstrated it on Youtube:
https://www.youtube.com/watch?v=qKsCnfOo0No


# This is an `n`-equal-tempered isomorphic synthesizer for the monome. Choose your `n`.

"Isomorphic" just means "the notes are equall spaced everywhere". Like if you tuned a guitar such that the space between each pair of adjacent strings was the same. The result is that any shape sound the same no matter where you put it, except transposed.

This was originally written to play only using 31 equally-spaced pitches per octave, with 6\31 (that is, six chromatic steps in the 31-tone-system) between each column of buttons. Now it lets you replace the values 31 and 6 with whatever you want. Just open the file `Math31.hs` and edit the definitions of `edo` and `spacing`.

The monome will light up all the enharmonic equivalents of the notes as you play them, which can be helpful for getting oriented. It can also permanently light up all enharmonic equivalents of some reference tone, as a reference -- the futuristic equivalent of putting a yellow sticker on every C on your piano keyboard. If you want that, ask me how, and I'll figure it out again.


# Installing it

You'll need to have already installed SuperCollider and Stack
(as in Haskell Stack).

Clone the project: `git clone https://github.com/JeffreyBenjaminBrown/monome`.This will create a folder,
referred to below as `monome/` or "the `monome/` folder".

From `monome/`, run `stack build`.


# Running it

By default, it assumes that your monome listens to the prefix "/monome",
receives at host 127.0.0.1, port 13993, and sends to host 127.0.0.1,
port 11111. If that's true, you'll only need to do what's described in
"If it is addressing correctly", below.
Otherwise you'll have to do what's described in
"Setting up its addressing", below that.


## If it is addressing correctly

Start SuperCollider. You can do this however you want;
one way is to run `bash supercollider.sh` from `monome/`.

Plug in your monome.

From `monome/`, run `stack ghci` to start a REPL.
(You'll need to have already installed Stack.)
The first time it might take a while, as it downloads things it needs.

From that REPL, run `et31`. The monome should light up,
and start responding to button presses with sounds.


## Setting up its addressing

Someday maybe this will be automatic,
but it's easy enough to do by hand for now;
it only requires a little human reading of the
[serialosc protocol](https://monome.org/docs/osc/),
which fortunately is very human-readable.

The OSC communcation occurs over "ports",
all of them on "localhost" (127.0.0.1).
[Serialosc](https://github.com/monome/serialosc),
the software that communicates between the monome and other stuff,
sends on address 12002. (There mimght be a way to change that if you need to;
you could ask those guys.)

From the `monome/` folder, start a REPL as before, by running `stack ghci`.
In that REPL, run the command `listenAndPrintOsc 8000`.
This will listen for OSC messages, at port 8000.
(You could use another value instead of 8000 if you want.)

Start a second REPL, and run this:
```
toSerialosc <- sendsTo (unpack localhost) 12002
send toSerialosc $ requestDeviceList 8000
```

The `listenAndPrintOsc` in the first REPL should now show something like this:
`OSC "/serialosc/device" [OSC_S "m0000102",OSC_S "monome 256",OSC_I 13993]`

That indicates there's a monome called `m0000102`, with 256 keys,
listening to port 13993. Suppose instead of 13993 it says 55555.
Then you'll need to replace every instance of 13993 in the code with 55555.
The easiest way to do that is to run this command from the `monome/` folder:
```
find src/ -type f -print0 | xargs -0 sed -i "s/13993/55555/g"
```

(This causes the code to send to port 55555.
An alternative solution would be to tell the monome to listen to port 13993;
if you want to do that, the serialosc protocol describes how.
Similar alternatives are available to the rest of this section.)

Next, after replacing that text,
we have to find out what prefix your monome is listening to.
(I don't know why; maybe two monomes can be listening on the same port?)
To do that, go to the second REPL (the one that's not running
`listenAndPrintOsc`), and run these two commands:
```
toMonome <- sendsTo (unpack localhost) 13993
send toMonome $ requestDeviceInfo 11111
```

(Rather than 13993,
use whatever the port is that your monome is actually listening on,
as found earlier.) The `listenAndPrintOsc`
in the first REPL should now show something like this:
```
OSC "/sys/id" [OSC_S "m0000102"]
OSC "/sys/size" [OSC_I 16,OSC_I 16]
OSC "/sys/host" [OSC_S "127.0.0.1"]
OSC "/sys/port" [OSC_I 11111]       # interesting
OSC "/sys/prefix" [OSC_S "/monome"] # interesting
OSC "/sys/rotation" [OSC_I 0]
```

Ignore the stuff not marked "interesting".
"port" is the port the monome sends to,
and "prefix" is the prefix the monome listens to. If "port" is, say,
55555 instead of 11111, run another substitution from the `monome/` folder:
```
find src/ -type f -print0 | xargs -0 sed -i "s/11111/55555/g"
```

Last, if the prefix is "/golem" instead of "/monome",
run this substitution fromthe `monome/` folder:
```
find src/ -type f -print0 | xargs -0 sed -i "s/\/monome/\/golem/g"
```

At this point,
you should be able to do what's described in the previous section,
"If it is addressing correctly".


# To extend it

It doesn't listen to most of the serialosc protocol.
You can find that [here](https://monome.org/docs/osc/).

Synthesis requires no knowledge of SuperCollider;
it is handled entirely within Haskell,
via the [Vivid library](http://hackage.haskell.org/package/vivid).
