-- For profiling purposes.

{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Lens
import Control.Concurrent (killThread)
import qualified Data.Map as M

import Vivid
import VividDispatch


main :: IO ()
main = do
  putStrLn $ "\nPress any key to quit. " ++
    "(You'll probably want to wait until you hear something first.)"
  disp <- newDispatch
  tid <- startDispatchLoop disp

  let chAll = replaceAll disp         -- change everything
      hush = replaceAll disp M.empty  -- stop (and lose) everything
      off = killThread tid >> freeAll -- kill the program

      p1 = stack2 a b & dur .~ 4 where
        a = mmho 3 $ pre2 "a" [(0, m1 "freq" 0)
                              ,(1, m1 "freq" 2)]
        b = mmho 2 $ pre2 "b" [(0, m1 "on" 0)
                              ,(1, m1 "freq" 4)]

      p2 :: Int -> Museq String Msg = \n ->
        mmho (fromIntegral n) $ pre2 "a" $
        zip (map RTime [0..]) $
        map (M.singleton "freq" . (+) 0) $
        map fromIntegral [0..n-1]
      
      go = nBoop . toHz . rootScale rs where
        toHz = ops [("freq", (*) 200 . \p -> 2**(p/12))]
        rs = slow 12 $ mmh 3 $ pre2 "a" $ [ (0, (0, phr3))
                                          , (1, (4, lyd))
                                          , (2 :: RTime, (1 :: Float, lyd7))
                                          ]
      
      wave = slow 4 $ mmh 2 $ pre2 "a" $ zip (map RTime [0..]) $
             [id, early $ 1]

  chAll $ mfl [
    ("1", go $ fast 2 $ merge0fa (slow 4 $ p2 4) $ merge0fa (p2 4) p1)
    , ("2", go $ fast 4 $ meta wave $
            merge0fa (slow 8 $ p2 4) $ merge0fa (p2 3) $
            cat [p1, p2 3, dense 2 p1])
      -- if I delete the "p2 3" from this last line,
      -- it takes effect immediately. (Using two instances of append
      -- instead of one cat has no effect.)
    ]

  _ <- getLine
  hush
  off
