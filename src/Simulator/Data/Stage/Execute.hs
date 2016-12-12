module Simulator.Data.Stage.Execute
  ( Execute(..)
  , newExecute
  , newSubPipeline
  ) where

import Data.Int

import Simulator.Data.Stall
import Simulator.Data.Registers
import Simulator.Data.Instruction

data Execute = Execute
  { _executeStalled      :: Stalled
  , _executeBypassValues :: Maybe [ (Int, Register, Int32) ]
  , _executeSubPipeline  :: [ [ (Int, InstructionVal, [ Register ]) ] ]
  }
  deriving (Show, Eq, Read)

newExecute :: Int -> Execute
newExecute n = Execute
  { _executeStalled      = newStalled
  , _executeBypassValues = Nothing
  , _executeSubPipeline  = newSubPipeline n
  }

newSubPipeline :: Int -> [ [ (Int, InstructionVal, [ Register ]) ] ]
newSubPipeline n = replicate n $ []
