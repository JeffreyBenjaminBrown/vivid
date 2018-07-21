module Vivid.Jbb.Random.MentionsSig where

import Vivid
import Vivid.Jbb.Random.Signal
import Vivid.Jbb.Random.Synth


class RenderSig a where
  renderSig :: (M.Map AbSigName a) -> a -> SDBody TheRParams Signal

--instance RenderSig AbSigName where
--  renderSig theMap sigName = 

instance RenderSig AbParam where
  renderSig _ AP1 = (V :: V 'AP1)
  renderSig _ AP2 = (V :: V 'AP2)
  renderSig _ AP3 = (V :: V 'AP3)
  renderSig _ AP4 = (V :: V 'AP4)
  renderSig _ AP5 = (V :: V 'AP5)
  renderSig _ AP6 = (V :: V 'AP6)
  renderSig _ AP7 = (V :: V 'AP7)
  renderSig _ AP8 = (V :: V 'AP8)
