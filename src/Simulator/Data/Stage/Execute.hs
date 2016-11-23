module Simulator.Data.Stage.Execute
  ( Execute(..)
  , newExecute
  ) where

import Data.Int
import Simulator.Data.Registers

data Execute = Execute
  { _executeStalled      :: Bool
  , _executeBypassValues :: Maybe [ (Register, Int32) ]
  }
  deriving (Show, Eq, Read)

newExecute :: Execute
newExecute = Execute
  { _executeStalled      = False
  , _executeBypassValues = Nothing
  }
