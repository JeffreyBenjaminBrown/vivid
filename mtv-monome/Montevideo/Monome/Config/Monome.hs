{-# LANGUAGE OverloadedStrings #-}

module Montevideo.Monome.Config.Monome where

import Montevideo.Monome.Types


type Port = Int

monomePort :: MonomeId -> Port

monomePort Monome_256 = 17800
monomePort Monome_128 = 17742
monomePort Monome_old = 19245
