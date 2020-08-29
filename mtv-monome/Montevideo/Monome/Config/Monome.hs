{-# LANGUAGE OverloadedStrings #-}

module Montevideo.Monome.Config.Monome where

import Montevideo.Monome.Types


type Port = Int

monomePort :: MonomeId -> Port
monomePort Monome_256 = 15226
monomePort Monome_128 = 14336
