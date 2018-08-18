module Vivid.Jbb.Dispatch.Instances where

import Control.DeepSeq

import Vivid
import Vivid.Jbb.Dispatch.Types
import Vivid.Jbb.Synths


instance NFData (Synth a) where
  rnf s = _unNodeId (_unSynth s) `deepseq` ()

instance NFData SynthRegister3 where
  rnf r = _boops3 r `deepseq` _vaps3 r `deepseq` _sqfms3 r `deepseq` ()

instance NFData a => NFData (Museq a) where
  rnf m = _dur m `deepseq` _sup m `deepseq` _vec m `deepseq` ()

instance NFData Action where
  rnf (New s n)    = s `deepseq` n `deepseq` ()
  rnf (Free s n)   = s `deepseq` n `deepseq` ()
  rnf (Send s n m) = s `deepseq` n `deepseq` m `deepseq` ()

instance NFData SynthDefEnum where
  rnf a = a `seq` ()
