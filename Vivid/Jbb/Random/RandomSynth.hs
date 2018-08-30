{-# LANGUAGE ViewPatterns #-}

module Vivid.Jbb.Random.RandomSynth where

import qualified Data.Map as M

import Vivid
import Vivid.Jbb.Random.Types
import Vivid.Jbb.Random.RandomSignal
import Vivid.Jbb.Random.MentionsSig
import Vivid.Jbb.Util (unique)


randAbSynth :: RandConstraints -> IO AbSynth
randAbSynth cs = -- TODO : prune cs <$>
  go cs M.empty where
  go :: RandConstraints -> AbSynth -> IO AbSynth
  go cs m = if namedSignals cs >= maxSignals cs
            then return m
            else do s <- randAbSig cs
                    let namedSignals' = namedSignals cs + 1
                        cs' = cs {namedSignals = namedSignals'}
                        m' = M.insert (sigName cs' namedSignals') s m
                    go cs' m'

-- TODO : debug `prune`; it seems to strip everything but the last
-- | After pruning, every remaining signal influences the last one
prune :: RandConstraints -> AbSynth -> AbSynth
prune cs m =
  let theUnused = unused cs [maximum $ M.keys m] m
      deleteKeys :: Ord k => [k] -> M.Map k a -> M.Map k a
      deleteKeys ks m = foldl (flip M.delete) m ks
  in deleteKeys (M.keys theUnused) m

-- | Produces an AbSynth containing only the unused signals
unused :: RandConstraints -> [AbSigName] -> AbSynth -> AbSynth
unused cs [] all = all
unused cs (u:used) m =
  let newMentions = allMentions cs u
      m' = M.delete u m
      remainingLeads = unique
        $ filter (flip elem $ M.keys m) -- delete irrelevant keys
        $ newMentions ++ used
  in unused cs remainingLeads m'

sigName :: RandConstraints -> Int -> AbSigName
sigName (namedSignals -> k) n =
  if n > 0 && n <= min k 8
  then theAbSigNames !! (n - 1)
  else error $ show n ++ " is not the number of an AbSigName."
