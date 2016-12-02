module Simulator.Data.Stage.Execute
  ( Execute(..)
  , newExecute
  ) where

import Data.Int
import Simulator.Data.Stall
import Simulator.Data.Registers
import Simulator.Data.Instruction

data Execute = Execute
  { _executeStalled        :: Stalled
  , _executeBypassValues   :: Maybe [ (Register, Int32) ]
  , _executeSubPipeline    :: [ (InstructionVal, [ Register ]) ]
  , _executeBranchInFlight :: Bool
  }
  deriving (Show, Eq, Read)

newExecute :: Execute
newExecute = Execute
  { _executeStalled        = newStalled
  , _executeBypassValues   = Nothing
  , _executeSubPipeline    = []
  , _executeBranchInFlight = False
  }
