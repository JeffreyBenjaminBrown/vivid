module Dispatch.Instances where

import Control.DeepSeq

import Vivid
import Dispatch.Types
import Synths


instance NFData (Synth a) where
  rnf s = _unNodeId (_unSynth s) `deepseq` ()

instance NFData SynthRegister where
  rnf r = _boops r `deepseq` _vaps r `deepseq` _sqfms r `deepseq` ()

instance NFData a => NFData (Museq a) where
  rnf m = _dur m `deepseq` _sup m `deepseq` _vec m `deepseq` ()

instance NFData Action where
  rnf (New s n)    = s `deepseq` n `deepseq` ()
  rnf (Free s n)   = s `deepseq` n `deepseq` ()
  rnf (Send s n m) = s `deepseq` n `deepseq` m `deepseq` ()

instance NFData SynthDefEnum where
  rnf a = a `seq` ()
