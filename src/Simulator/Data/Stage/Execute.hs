module Simulator.Data.Stage.Execute
  ( Execute(..)
  , newExecute
  ) where

import Data.Int
import Simulator.Data.Stall
import Simulator.Data.Registers

data Execute = Execute
  { _executeStalled      :: Stalled
  , _executeBypassValues :: Maybe [ (Register, Int32) ]
  }
  deriving (Show, Eq, Read)

newExecute :: Execute
newExecute = Execute
  { _executeStalled      = newStalled
  , _executeBypassValues = Nothing
  }
