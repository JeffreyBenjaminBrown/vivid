-- example by Tom Murphy:
-- https://we.lurk.org/hyperkitty/list/livecode@we.lurk.org/thread/6JD5SHXPQQ25VZH4PVKIR5Y7HEAPXZWL/

{-# LANGUAGE DataKinds, ExtendedDefaultRules #-}

import Vivid

foo = sd (0 :: I "buf") $ do
   let buf = V::V "buf"
   s <- playBuf (buf_ buf, rate_ $ bufRateScale buf ~* 3, doneAction_ 2)
   out 0 [s,s]

main = do
   buf <- newBufferFromFile "the_letter.flac"
   synth foo (b2i buf :: I "buf")
