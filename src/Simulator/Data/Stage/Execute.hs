module Simulator.Data.Stage.Execute
  ( Execute(..)
  , newExecute
  ) where

import Data.Int
import Simulator.Data.Stall
import Simulator.Data.Registers
import Simulator.Data.Instruction

data Execute = Execute
  { _executeStalled      :: Stalled
  , _executeBypassValues :: Maybe [ (Register, Int32) ]
  , _executeSubPipeline  :: Maybe [ InstructionVal ]
  }
  deriving (Show, Eq, Read)

newExecute :: Bool -> Execute
newExecute sub_pipeline = Execute
  { _executeStalled      = newStalled
  , _executeBypassValues = Nothing
  , _executeSubPipeline  = if sub_pipeline
                             then Just []
                             else Nothing
  }
