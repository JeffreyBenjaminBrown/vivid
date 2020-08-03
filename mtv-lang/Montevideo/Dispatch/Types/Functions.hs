{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE TupleSections #-}

-- | These aren't type defnitions, but they're such simple functions
-- (in many cases constructors or instances)
-- that I feel like they belong in Montevideo.Dispatch.Types.

module Montevideo.Dispatch.Types.Functions (
  -- | * Names
    mNamed -- ^ n -> a -> NamedWith (Maybe n) a
  , anon -- ^ a -> NamedWith (Maybe n) a

  -- | * ScActions
  , actionToSynth -- ^ ScAction -> (SynthDefEnum, SynthName)

  -- | * Events
  , eventRTimeToEventTime -- ^ Event RTime l a -> Event Time l a
  , evStart, evEnd -- ^ Lens' (Event t l a) t
  , showEvs -- ^ (Foldable t, Show a, Show label)
            --   => t (Ev label a) -> String
  , mkEv  -- ^ l -> Rational -> Rational -> a -> Ev l a
  , mkEv0 -- ^ l -> Rational -> a -> Ev l a

  -- | * `Museq`s
  , Museq(..) -- ^ the Functor instance
  , emptyMuseq -- ^ Museq label a

  -- | * `SynthRegister`
  , emptySynthRegister -- ^ SynthRegister

  -- | * `Dispatch`
  , ShowIO(..)
  , Dispatch(..)
  , newDispatch -- ^ IO Dispatch
  ) where

import Control.Concurrent.MVar
import Control.Lens hiding (anon)
import qualified Data.Map as M
import qualified Data.Vector as V

import Montevideo.Util
import Montevideo.Synth
import Montevideo.Dispatch.Types.Time
import Montevideo.Dispatch.Types.Many


-- | * Names

mNamed :: n -> a -> NamedWith (Maybe n) a
mNamed n = (Just n , )

anon :: a -> NamedWith (Maybe n) a
anon = (Nothing , )


-- | * ScActions

-- | From an action, extract the synth it is for.
actionToSynth :: ScAction a -> (SynthDefEnum, a)
actionToSynth (ScAction_New  s n _) = (s,n)
actionToSynth (ScAction_Free s n  ) = (s,n)
actionToSynth (ScAction_Send s n _) = (s,n)


-- | * Events

eventRTimeToEventTime :: Event RTime l a -> Event Time l a
eventRTimeToEventTime ev =
  let (s,t) = _evArc ev
  in ev { _evArc = (tr s, tr t) }

evStart, evEnd :: Lens' (Event t l a) t
evStart = evArc . _1
evEnd   = evArc . _2

showEvs :: (Foldable t, Show a, Show label)
        => t (Ev label a) -> String
showEvs = foldl (\acc ev -> acc ++ show ev) "\n"

mkEv :: l -> Rational -> Rational -> a -> Ev l a
mkEv l s e a = Event l (fr s, fr e) a

-- ^ make a duration-0 event
mkEv0 :: l -> Rational -> a -> Ev l a
mkEv0 l t a = Event l (fr t, fr t) a


-- | * `Museq`s

instance Functor (Museq label) where
  fmap = over vec . V.map . over evData

emptyMuseq :: Museq label a
emptyMuseq = Museq { _dur = 1, _sup = 1, _vec = V.empty }


-- | * `SynthRegister`

emptySynthRegister :: SynthRegister
emptySynthRegister = SynthRegister
  { _boops = mempty
  , _vaps = mempty
  , _samplers = mempty
  , _samples = mempty
  , _sqfms = mempty
  , _zots = mempty }


-- | * `Dispatch`

newDispatch :: IO Dispatch
newDispatch = do
  a <- newMVar M.empty
  b <- newMVar emptySynthRegister
  c <- newEmptyMVar
  d <- newMVar 1
  return Dispatch { mMuseqs      = a
                  , mReg         = b
                  , mTime0       = c
                  , mTempoPeriod = d }

class ShowIO a where
  showIO :: a -> IO String

instance ShowIO Dispatch where
  showIO d = do
    m <- readMVar $ mMuseqs d
    -- r <- readMVar $ mReg d -- The synth register is rarely of interest,
                              -- and it occupies a lot of screen space.
    t <- readMVar $ mTime0 d
    p <- readMVar $ mTempoPeriod d
    return $ unlines $ map show $
      zip ["museqs", "time0", "period"] $
      [show m, show t, show p]
