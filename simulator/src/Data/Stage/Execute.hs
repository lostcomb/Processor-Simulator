module Data.Stage.Execute
  ( Execute(..)
  , Execute_T(..)
  , newExecute
  ) where

import Data.Int
import Data.Stage
import Data.Registers
import Control.Monad.State

data Execute_T = Execute_T
  { _stalled            :: Bool
  , _no_execution_units :: Int
  , _bypass_values      :: [ (Register, Int32) ]
  }
  deriving (Show, Eq, Read)

instance Stage Execute_T where
  stall    e = e { _stalled = True  }
  continue e = e { _stalled = False }
  stalled  e = _stalled e

newExecute :: Int -> Execute_T
newExecute n = Execute_T
  { _stalled            = False
  , _no_execution_units = n
  , _bypass_values      = []
  }

class Execute e where
  getBypassValues     :: State e [ (Register, Int32) ]
  setBypassValues     :: [ (Register, Int32) ] -> State e ()
  getNoExecutionUnits :: State e Int
