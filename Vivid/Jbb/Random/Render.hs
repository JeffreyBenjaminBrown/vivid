-- aw dang, I have no idea

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


type TheRParams = '["AP1", "AP2", "AP3", "AP4", "AP5", "AP6", "AP7", "AP8"]
type RenderTarget = SDBody' TheRParams Signal
  -- ^ I hope to turn abstract signals into this type.


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

instance RenderSig AbParam where -- TODO ? a goofy hack, but it's cpu-cheap.
  renderSig _ AP1 = (V :: V "AP1") ~+ (0 ~* pulse (freq_ 1))
  renderSig _ AP2 = (V :: V "AP2") ~+ (0 ~* pulse (freq_ 1))
  renderSig _ AP3 = (V :: V "AP3") ~+ (0 ~* pulse (freq_ 1))
  renderSig _ AP4 = (V :: V "AP4") ~+ (0 ~* pulse (freq_ 1))
  renderSig _ AP5 = (V :: V "AP5") ~+ (0 ~* pulse (freq_ 1))
  renderSig _ AP6 = (V :: V "AP6") ~+ (0 ~* pulse (freq_ 1))
  renderSig _ AP7 = (V :: V "AP7") ~+ (0 ~* pulse (freq_ 1))
  renderSig _ AP8 = (V :: V "AP8") ~+ (0 ~* pulse (freq_ 1))
