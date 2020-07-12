{-# LANGUAGE ScopedTypeVariables #-}

module Montevideo.Dispatch.Museq (
  -- | = Naming things
    labelsToStrings         -- ^ Show l => Museq l a -> Museq String a
  , museqMaybeNamesAreValid -- ^ forall l a. Eq l
                            -- => Museq (Maybe l) a -> Bool
  , nameAnonEvents -- ^ forall a.
                   --    Museq (Maybe String) a
                   -- -> Museq        String  a
  , intNameEvents -- ^ RDuration -- ^ _sup of the Museq these Evs come from
                  -- -> [Ev () a] -- ^ these are being named
                  -- -> [Ev Int a]

  -- | = analyze a `Museq`
  , museqSynths -- ^ Museq String Note -> [(SynthDefEnum, SynthName)]
  , museqSynthsDiff -- ^ M.Map MuseqName (Museq String Note)
               -- -> M.Map MuseqName (Museq String Note)
               -- -> ([(SynthDefEnum, SynthName)],
               --     [(SynthDefEnum, SynthName)])
  , museqIsValid -- ^ (Eq a, Eq l) => Museq l a -> Bool

  -- | = misc
  , sortMuseq    -- ^ Museq l a -> Museq l a
  , museq_NotesToActions -- ^ M.Map String (Museq String Note) ->
                         --   M.Map String (Museq String Action)
  ) where

import Prelude hiding (cycle)

import Control.Lens hiding (to,from)
import Control.Monad.ST
import Data.Function (on)
import qualified Data.List as L
import qualified Data.Maybe as Mb
import qualified Data.Map as M
import qualified Data.Vector as V
import Data.Vector.Algorithms.Intro (sortBy)

import Montevideo.Dispatch.Types
import Montevideo.Synth
import Montevideo.Util


-- | = Naming things

labelsToStrings :: Show l => Museq l a -> Museq String a
labelsToStrings = over vec $ V.map $ over evLabel show

-- | The names in a `Museq Name` are valid if no events with the same
-- name overlap in time, within or across cycles.
museqMaybeNamesAreValid :: forall l a. Eq l
                         => Museq (Maybe l) a -> Bool
museqMaybeNamesAreValid m = and $ map goodGroup nameGroups where
  namedEvents = filter (Mb.isJust . view evLabel) $ V.toList $ _vec m
  nameGroups = L.groupBy ((==) `on` view evLabel) namedEvents
    :: [[Ev (Maybe l) a]]
  adjacentOverlap, wrappedOverlap, goodGroup
    :: [Ev (Maybe l) a] -> Bool
  adjacentOverlap [] = False
  adjacentOverlap (_:[]) = False
  adjacentOverlap (e : f : more) = overlap (e^.evArc) (f^.evArc)
                                   || adjacentOverlap (f:more)
  wrappedOverlap evs = overlap (last evs ^. evArc)
                       (bumpArc (_sup m) $ head evs ^. evArc)
  goodGroup g = not $ adjacentOverlap g || wrappedOverlap g

nameAnonEvents :: forall a.
                   Museq (Maybe String) a
                -> Museq        String  a
nameAnonEvents m =
  sortMuseq $ m {_vec = V.fromList $ namedEvs' ++ namedAnons}
  where
    evs, namedEvs, anonEvs :: [Ev (Maybe String) a]
    evs = V.toList $ _vec m
    (namedEvs, anonEvs) = L.partition (Mb.isJust . view evLabel) evs
    namedEvs' = map (over evLabel Mb.fromJust) namedEvs :: [Ev String a]
    names = map (view evLabel) namedEvs' :: [String]
    anonEvs' = map (over evLabel $ const ()) anonEvs :: [Ev () a]
    intNamedAnons = intNameEvents (_sup m) anonEvs' :: [Ev Int a]
    namedAnons = map (over evLabel f) intNamedAnons :: [Ev String a]
      where f = ((++) $ unusedName names) . show
            -- The ++ ensures no name conflicts.

-- | Assign a minimal number of names
-- (integers here, but they probably become strings downstream)
-- starting from 1, so that like-named events do not overlap.
-- PITFALL: ASSUMES the input list is sorted on (start,end) times.
intNameEvents :: RDuration -- ^ _sup of the Museq these Evs come from
              -> [Ev () a] -- ^ These are being named.
              -> [Ev Int a]
intNameEvents sup0 (ev1:more) =
  ev1' : _intNameEvents sup0 ev1' [ev1'] more
  where ev1' = over evLabel (const 1) ev1
intNameEvents _ _ = error "intNameEvents: uncaught input pattern."

_intNameEvents :: forall a.
  RDuration -- ^ _sup of the Museq these Evs come from
  -> (Ev Int a) -- ^ the first event
  -> [Ev Int a] -- ^ ongoing events
  -> [Ev ()  a] -- ^ these are being named
  -> [Ev Int a]
_intNameEvents _ _ _ [] = []
_intNameEvents sup0 ev1 ongoing (ev : more) =
  over evLabel (const name) ev
  : _intNameEvents sup0 ev1 ongoing' more
  where
    ongoing' = filter (\ev' -> overlap (ev ^. evArc) (ev' ^. evArc)) ongoing
      -- ongoing in the sense that they do not end before ev starts
    firstOverlaps = overlap (bumpArc sup0 $ ev1 ^. evArc) (ev ^. evArc)
    overlappingMaybeNames = if firstOverlaps
                            then (ev1 ^. evLabel) : ns
                            else                    ns
      where ns = map (view evLabel) ongoing'
    name = head $ (L.\\) [1..] overlappingMaybeNames


-- | = Analyze a `Museq`

-- | Given an old collection of Museqs and a new one, figure out
-- which synths need to be created, and which destroyed.
-- PITFALL: Both resulting lists are ordered on the first element,
-- likely differing from either of the input maps.
museqSynthsDiff :: M.Map MuseqName (Museq String Note) -- ^ old
           -> M.Map MuseqName (Museq String Note) -- ^ new
           -> ( [(SynthDefEnum, SynthName)],  -- ^ create these
                [(SynthDefEnum, SynthName)] ) -- ^ destroy these
museqSynthsDiff old new = (toFree,toCreate) where
  oldMuseqs = M.elems old :: [Museq String Note]
  newMuseqs = M.elems new :: [Museq String Note]
  oldSynths = unique $ concatMap museqSynths oldMuseqs
  newSynths = unique $ concatMap museqSynths newMuseqs
  toCreate = (L.\\) newSynths oldSynths
  toFree = (L.\\) oldSynths newSynths

-- | Given a Museq, find the synths it uses.
museqSynths :: Museq String Note -> [(SynthDefEnum, SynthName)]
museqSynths m = map f evs where
  evs = V.toList $ _vec m :: [Ev String Note]
  f :: Ev String Note -> (SynthDefEnum, SynthName)
  f ev = ( view (evData . noteSd) ev
         , view evLabel ev )

-- | A valid Museq m is sorted on start and then end times,
-- with all end times >= the corresponding start times,
-- has (relative) duration > 0, and all events at time < _sup m.
-- TODO ? I'm not sure the end-time sort helps.
-- PITFALL : The end times can be greater than _sup.
-- That's important, as you might want events from one cycle
-- to reach into the next.
museqIsValid :: (Eq a, Eq l) => Museq l a -> Bool
museqIsValid mu = and [a,b,c,d,e] where
  v = _vec mu
  a = if V.length v == 0 then True
      else view evEnd (V.last v) < _sup mu
  b = mu == sortMuseq mu
  c = _dur mu > 0
  d = _sup mu > 0
  e = V.all (uncurry (<=) . view evArc) v


-- | = Misc

sortMuseq :: Museq l a -> Museq l a
sortMuseq = vec %~
  -- V.Vector is immuatble. V.thaw gives a mutable one,
  -- which is what Vector.Algorithms.Intro.sortBy needs.
  \v -> runST $ do v' <- V.thaw v
                   sortBy (compare `on` view evArc) v'
                   V.freeze v'

museq_NotesToActions ::
  M.Map String (Museq String Note) ->
  M.Map String (Museq String Action)
museq_NotesToActions mqs = let
  f :: Ev String Note -> Ev String Action
  f ev = evData .~ act $ ev where
    d = ev ^. evData
    act = Send (d^.noteSd)
          (ev^.evLabel) -- TODO ? awkward : Ev label is repeated in Action
          (d^.noteMsg)
  in M.map (vec %~ V.map f) mqs
