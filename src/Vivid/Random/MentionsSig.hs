module Vivid.Random.MentionsSig where

import Vivid.Random.Types


class MentionsSig a where
  mentionsSig :: AbSigName -> a -> Bool

allMentions :: MentionsSig a => RandConstraints -> a -> [AbSigName]
allMentions cs a = let names = take (maxSignals cs) theAbSigNames
                   in filter (\y -> mentionsSig y a) names

instance MentionsSig AbSig where
  mentionsSig name (AbSigFormula f) = mentionsSig name f
  mentionsSig name (AbSigGen g) = mentionsSig name g
  mentionsSig name (AbSig n) = mentionsSig name n
  mentionsSig name (AbV p) = mentionsSig name p
  mentionsSig name (AbConst f) = False

instance MentionsSig AbFormula where
  mentionsSig name (AbProd x y) = mentionsSig name x || mentionsSig name y
  mentionsSig name (AbSum x y)  = mentionsSig name x || mentionsSig name y

instance MentionsSig AbGen where
  mentionsSig name (AbSin x) =   mentionsSig name x
  mentionsSig name (AbSaw x) =   mentionsSig name x

instance MentionsSig AbSinMsg where
  mentionsSig name (AbSinMsg x y) = mentionsSig name x || mentionsSig name y

instance MentionsSig AbSawMsg where
  mentionsSig name (AbSawMsg x) = mentionsSig name x

instance MentionsSig AbSigName where
  mentionsSig = (==)

instance MentionsSig AbParam where
  mentionsSig _ _ = False
