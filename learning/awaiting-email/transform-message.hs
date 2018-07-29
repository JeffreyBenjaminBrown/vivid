-- Asking on Haskell-art: "Filter a message based on its type (parameter)?"

{-# LANGUAGE -- DataKinds, ExtendedDefaultRules,
ScopedTypeVariables #-}

import Vivid

f :: forall inParams outParams.
     ( VarList inParams, VarList outParams
     , Subset (InnerVars inParams) '["freq","amp"]
     , Subset (InnerVars outParams) '["whack"] )
  => inParams -> outParams
f _ = ()

f () = ()
