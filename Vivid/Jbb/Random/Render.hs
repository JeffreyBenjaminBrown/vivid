-- aw dang, I have no idea

{-# LANGUAGE DataKinds
           , ExtendedDefaultRules
           , FlexibleContexts
           , ConstrainedClassMethods
           #-}

module Vivid.Jbb.Random.Render where

import qualified Data.Map as M

import Vivid
import Vivid.Jbb.Random.Types
import Vivid.Jbb.Random.RandomSignal
import Vivid.Jbb.Random.RandomSynth


type TheRParams = '["AP1", "AP2", "AP3", "AP4", "AP5", "AP6", "AP7", "AP8"]

class RenderSig a where
  renderSig :: ToSig a TheRParams
            => (M.Map AbSigName a) -> a -> SDBody a Signal

--instance RenderSig AbParam where
--  renderSig _ AP1 = (V :: V "AP1")
--  renderSig _ AP2 = (V :: V "AP2")
--  renderSig _ AP3 = (V :: V "AP3")
--  renderSig _ AP4 = (V :: V "AP4")
--  renderSig _ AP5 = (V :: V "AP5")
--  renderSig _ AP6 = (V :: V "AP6")
--  renderSig _ AP7 = (V :: V "AP7")
--  renderSig _ AP8 = (V :: V "AP8")
