-- | This illustrates how to re-trigger a buffer.
-- (Another, perhaps more standard, idiom is to free the synth
-- after playing the buffer.)

{-# LANGUAGE DataKinds, ExtendedDefaultRules #-}

import Vivid


beep :: SynthDef '["buf", "trigger"]
beep = sd ( 0 :: I "buf"
          , 1 :: I "trigger") $ do
   let buf = V::V "buf"
   s <- playBuf
     ( trigger_ (V::V"trigger")
     , buf_ buf
     , rate_ $ bufRateScale buf ~* (1::Float)
     , doneAction_ (0::Int) -- with this, `s` persists
       -- after it finishes playing. (With 2 instead of 0,
       -- it would disappear.)
     )
   out (0::Int) [s,s]

main :: IO ()
main = do
  buf <- newBufferFromFile $ "/home/jeff/code/Tidal/Dirt-Samples/latibro/000_Sound2.wav"
  s <- synth beep ( b2i buf :: I "buf")
  set s (0 :: I "trigger") -- get ready to be retriggered.
  wait (1/2::Float) -- without this wait, the first beep
    -- will be interrupted by the second before there's time to hear it.
  set s (1 :: I "trigger") -- whenever "trigger" goes from 0 to greater
    -- than 0, the buffer is replayed. (But some amount of time must
    -- be allowed to elapse between them.)
  wait (1/2::Float) -- without this wait, the synth would disappear
    -- before the second beep was heard.
  free s
