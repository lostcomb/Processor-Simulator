module Data.Stage.Fetch
  ( Fetch_T(..)
  , Fetch(..)
  , newFetch
  ) where

import Data.Word
import Data.Stage
import Control.Monad.State

data Fetch_T = Fetch_T
  { _stalled         :: Bool
  , _no_of_insts     :: Int
  , _program_counter :: Word32
  }
  deriving (Show, Eq, Read)

instance Stage Fetch_T where
  stall    f = f { _stalled = True  }
  continue f = f { _stalled = False }
  stalled  f = _stalled f

newFetch :: Int -> Fetch_T
newFetch n = Fetch_T
  { _stalled         = False
  , _no_of_insts     = n
  , _program_counter = 0
  }

class Fetch f where
  getNoOfInsts      :: State f Int
  getProgramCounter :: State f Word32
  setProgramCounter :: Word32 -> State f ()
