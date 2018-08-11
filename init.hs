:set prompt "> "
:set -XDataKinds
import Control.Lens
import Control.Concurrent
import Control.Concurrent.MVar
import qualified Data.Map as M
import qualified Data.Vector as V
:def! . readHsAsGhci
