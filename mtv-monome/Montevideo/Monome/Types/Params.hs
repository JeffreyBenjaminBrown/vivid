{-# LANGUAGE TemplateHaskell #-}

module Montevideo.Monome.Types.Params where

import           Control.Lens
import           Data.Either.Combinators
import qualified Data.Bimap as Bi

import           Montevideo.Monome.Types.Monome


-- | Two parameters are omitted: "on", which is only ever 1
-- (Dispatch needs it, I think, but Monome doesn't),
-- and "freq", which is controlled by the keyboard, not the "sliders".
data ParamGroup
  = PG_FM     -- ^ fm-m, fm-f, fm-b
  | PG_PM     -- ^ pm-m, pm-f, pm-b,
  | PG_WM     -- ^ wm-m, wm-f, wm-b, w
  | PG_source -- ^ amp, pulse
  | PG_AM     -- ^ am, am-b, am-f
  | PG_RM     -- ^ rm, rm-b, rm-f
  | PG_HLF    -- ^ hpf, hpf-m, lpf,  lpf-m
  | PG_BF     -- ^ bpf, bpf-m, bpf-q
  | PG_end    -- ^ lim, sh, sh-b, del
  deriving (Eq, Ord, Show)
makePrisms ''ParamGroup

paramGroupStrings :: ParamGroup -> [String]
paramGroupStrings PG_FM     = ["fm-m", "fm-f", "fm-b"] 
paramGroupStrings PG_PM     = ["pm-m", "pm-f", "pm-b,"] 
paramGroupStrings PG_WM     = ["wm-m", "wm-f", "wm-b", "w"] 
paramGroupStrings PG_source = ["amp", "pulse"] 
paramGroupStrings PG_AM     = ["am", "am-b", "am-f"] 
paramGroupStrings PG_RM     = ["rm", "rm-b", "rm-f"] 
paramGroupStrings PG_HLF    = ["hpf", "hpf-m", "lpf", "lpf-m"] 
paramGroupStrings PG_BF     = ["bpf", "bpf-m", "bpf-q"] 
paramGroupStrings PG_end    = ["lim", "sh", "sh-b, del"] 

paramGroup_toParam :: ParamGroup -> Int -> Either String String
paramGroup_toParam pg i =
  mapLeft ("paramGroup_toParam: " ++) $
  case drop i $ paramGroupStrings pg of
    [] -> Left ( "ParamGroup " ++ show pg ++
                 " has fewer than " ++ show i ++ " parameters." )
    (s:_) -> Right s

-- | This is total in one direction; keying on a ParamGroup never fails.
-- (In that case it's clearer to use `paramGroup_toXy`, below.)
paramGroupXys :: Bi.Bimap ParamGroup (X,Y)
paramGroupXys = Bi.fromList
  [ (PG_FM     , (0,0))
  , (PG_PM     , (1,0))
  , (PG_WM     , (2,0))
  , (PG_source , (0,1))
  , (PG_AM     , (1,1))
  , (PG_RM     , (2,1))
  , (PG_HLF    , (0,2))
  , (PG_BF     , (1,2))
  , (PG_end    , (2,2))
  ]

paramGroup_toXy :: ParamGroup -> (X,Y)
paramGroup_toXy = (Bi.!) paramGroupXys
