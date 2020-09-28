-- | A window for changing what windows are on a monome.

module Montevideo.Monome.Window.Change (
  changewindow
  ) where

import           Prelude hiding (pred)
import           Control.Lens hiding (Choice)
import qualified Data.List as L

import           Montevideo.Monome.Types.Most
import           Montevideo.Util


type Choice app = [ ( (X,Y), Window app ) ]

label :: WindowId
label = ChangeWindow

changewindow
  :: [Choice app] -- ^ PITFALL: The choices should not include
  -- the ChangeWindow. (If they did the recursion would never stop.)
  -- Instead the handler finds the current ChangeWindow and puts it
  -- at the front of the new list.
  -> Window app
changewindow choices =  Window {
    windowLabel = label
  , windowContains = \(x,y) -> x == 0 &&
                               numBetween 0 (length choices - 1) y
  , windowInitLeds = \_ _ -> Right []
  , windowHandler = handler choices }

handler :: forall app.
           [Choice app]
        -> St app -> (MonomeId, ((X,Y), Switch))
        -> Either String (St app)
handler _       st (_ , (_    , False)) =
  Right st
handler choices st (mi, ((_,y), True )) = let
  c :: Choice app = choices !! y -- safe thanks to `windowContains` above
  Just layers = st ^. stWindowLayers . at mi
  Just (thisChangeWindow :: ((X,Y), Window app)) =
    L.find f layers where
           f         :: ((X,Y), Window app) -> Bool
           f (_,window) = windowLabel window == ChangeWindow
  in Right $ st
     & ( stWindowLayers . at mi . _Just %~
         const (thisChangeWindow : c) )
     & ( stPending_Monome %~
         flip (++) ( map ((mi,label),)
                     $ [ ((0,y'), False)
                       | y' <- [0 .. length choices - 1] ]
                     ++ [((0,y), True)] ) )
