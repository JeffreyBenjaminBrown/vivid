module Vivid.Jbb.Random.MentionsSig where

import Vivid
import Vivid.Jbb.Random.Signal


class MentionsSig a where
  mentionsSig :: AbSigName -> a -> Bool

--allMentions :: MentionsSig a => RandConstraints -> a -> [AbSigName]
--allMentions cs s =
--  let allNames = take (maxSignals cs) theAbSigNames

instance MentionsSig AbParam where
  mentionsSig _ _ = False

instance MentionsSig AbSigName where
  mentionsSig = (==)

instance MentionsSig AbSawMsg where
  mentionsSig name (AbSawMsg x) = mentionsSig name x

instance MentionsSig AbSinMsg where
  mentionsSig name (AbSinMsg x y) = mentionsSig name x || mentionsSig name y

instance MentionsSig AbGen where
  mentionsSig name (AbSin x) =   mentionsSig name x 
  mentionsSig name (AbSaw x) =   mentionsSig name x 

instance MentionsSig AbFormula where
  mentionsSig name (RProd x y) = mentionsSig name x || mentionsSig name y
  mentionsSig name (RSum x y)  = mentionsSig name x || mentionsSig name y

instance MentionsSig AbSig where
  mentionsSig name (AbSigFormula f) = mentionsSig name f
  mentionsSig name (AbSigGen g) = mentionsSig name g
  mentionsSig name (AbSig n) = mentionsSig name n
  mentionsSig name (AbV p) = mentionsSig name p
