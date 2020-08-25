{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE
UndecidableInstances
#-}

module Montevideo.Monome.Types.Instances where

import Montevideo.Monome.Types.Most


instance Eq (Window app) where
  (==) a b = windowLabel a == windowLabel b

instance Show (Window app) where
  show = show . windowLabel

instance Show (Pitch app) => Show (Voice app) where
  show v = "Voice "
    ++ "{ _voiceSynth = "  ++ show (_voiceSynth v)
    ++ "{ _voicePitch = "  ++ show (_voicePitch v)
    ++ "{ _voiceParams = " ++ show (_voiceParams v)

instance Eq (Pitch app) => Eq (Voice app) where
  a == b = and
    [ _voiceSynth a  == _voiceSynth b
    , _voicePitch a  == _voicePitch b
    , _voiceParams a == _voiceParams b ]

instance (Show app, Show (Pitch app)) => Show (St app) where
  show app = "St "
    ++ "{ _stApp app = "            ++ show (_stApp app)
    ++ ", _stWindowLayers app = "   ++ show (_stWindowLayers app)
    ++ ", _stToMonome app = "       ++ show (_stToMonome app)
    ++ ", _stVoices app = "         ++ show (_stVoices app)
    ++ ", _stPending_Monome app = " ++ show (_stPending_Monome app)
    ++ ", _stPending_Vivid app = "  ++ show (_stPending_Vivid app)

instance (Eq app, Eq (Pitch app)) => Eq (St app) where
  a == b = and
    [ _stApp a           == _stApp b
    , _stWindowLayers a  == _stWindowLayers b
    , _stToMonome a      == _stToMonome b
    , _stVoices a        == _stVoices b
    , _stPending_Monome a == _stPending_Monome b
    , _stPending_Vivid a  == _stPending_Vivid b ]

instance Num EdoPitch where
  (+) (EdoPitch a) (EdoPitch b) = EdoPitch $ a + b
  (*) (EdoPitch a) (EdoPitch b) = EdoPitch $ a * b
  (-) (EdoPitch a) (EdoPitch b) = EdoPitch $ a - b
  abs (EdoPitch a) = EdoPitch $ abs a
  signum (EdoPitch a) = EdoPitch $ signum a
  fromInteger a = EdoPitch $ fromIntegral a

instance Num EdoPitchClass where
  (+) (EdoPitchClass a) (EdoPitchClass b) = EdoPitchClass $ a + b
  (*) (EdoPitchClass a) (EdoPitchClass b) = EdoPitchClass $ a * b
  (-) (EdoPitchClass a) (EdoPitchClass b) = EdoPitchClass $ a - b
  abs (EdoPitchClass a) = EdoPitchClass $ abs a
  signum (EdoPitchClass a) = EdoPitchClass $ signum a
  fromInteger a = EdoPitchClass $ fromIntegral a
