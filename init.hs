:set prompt "> "
:set -XDataKinds
import qualified Control.Lens as Lens
import qualified Data.List as L
import Control.Concurrent
import Control.Concurrent.MVar
import qualified Data.Maybe as Mb
import qualified Data.Map as M
import qualified Data.Set as S
import qualified Data.Vector as V
import Data.Ratio
:def! . readHsAsGhci
