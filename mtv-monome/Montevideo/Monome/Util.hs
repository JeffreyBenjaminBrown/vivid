module Montevideo.Monome.Util (
    nextVoice     -- ^ M.Map VoiceId a -> VoiceId
  , monome_scActionNew -- ^ EdoConfig -> VoiceId -> M.Map ZotParam Float
                      -- -> EdoPitch -> ScAction VoiceId
  ) where

import qualified Data.Map as M

import           Montevideo.Dispatch.Types
import qualified Montevideo.Monome.Config.Mtv as Config
import qualified Montevideo.Monome.EdoMath as EM
import           Montevideo.Monome.Types
import           Montevideo.Synth


nextVoice :: St a -> VoiceId
nextVoice st =
  case M.lookupMax $ _stVoices st of
    Nothing             -> VoiceId 0
    Just (VoiceId i, _) -> VoiceId $ i+1

monome_scActionNew
  :: EdoConfig -> VoiceId -> M.Map ZotParam Float -> EdoPitch
  -> ScAction VoiceId
monome_scActionNew ec vi timbre pitch = ScAction_New
  { _actionSynthDefEnum = Zot
  , _actionSynthName = vi
  , _actionScMsg =
      M.mapKeys show -- show :: ZotParam -> String
      $ M.union -- in fonclict, the first arg takes priority
      ( M.fromList [ (Zot_freq, Config.freq *
                                EM.edoToFreq ec pitch) ] )
      $ timbre }
