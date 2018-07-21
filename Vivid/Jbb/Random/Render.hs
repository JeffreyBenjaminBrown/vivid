{-# LANGUAGE DataKinds
           , ExtendedDefaultRules
           , FlexibleContexts
           , ScopedTypeVariables
           , ConstrainedClassMethods
           #-}

module Vivid.Jbb.Random.Render where

import qualified Data.Map as M

import Vivid
import Vivid.Jbb.Random.Types
import Vivid.Jbb.Random.RandomSignal
import Vivid.Jbb.Random.RandomSynth


type RenderTarget = SDBody' TheAbParams Signal
  -- ^ Rendering turns abstract signals into this type.


class RenderSig a where
  renderSig :: M.Map AbSigName RenderTarget -> a -> RenderTarget

instance RenderSig AbSig where
  renderSig m (AbSigFormula abFormula) = renderSig m abFormula
  renderSig m (AbSigGen abGen) = renderSig m abGen
  renderSig m (AbSig abSigName) = renderSig m abSigName
  renderSig m (AbV abParam) = renderSig m abParam

instance RenderSig AbFormula where
  renderSig m (AbProd x y) = renderSig m x ~* renderSig m y
  renderSig m (AbSum x y) = renderSig m x ~+ renderSig m y

instance RenderSig AbGen where
  renderSig m (AbSin (AbSinMsg freq phase)) =
    sinOsc (freq_ $ renderSig m freq, phase_ $ renderSig m phase)
  renderSig m (AbSaw (AbSawMsg freq)) =
    saw (freq_ $ renderSig m freq)

instance RenderSig AbSigName where
  renderSig = (M.!)

instance RenderSig AbParam where
  renderSig _ AP1 = toSig (V :: V "AP1")
  renderSig _ AP2 = toSig (V :: V "AP2")
  renderSig _ AP3 = toSig (V :: V "AP3")
  renderSig _ AP4 = toSig (V :: V "AP4")
  renderSig _ AP5 = toSig (V :: V "AP5")
  renderSig _ AP6 = toSig (V :: V "AP6")
  renderSig _ AP7 = toSig (V :: V "AP7")
  renderSig _ AP8 = toSig (V :: V "AP8")
