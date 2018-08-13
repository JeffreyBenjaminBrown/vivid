:set prompt "> "
:set -XDataKinds
import qualified Control.Lens as L
import Control.Concurrent
import Control.Concurrent.MVar
import qualified Data.Map as M
import qualified Data.Vector as V
import Data.Ratio
:def! . readHsAsGhci
