{-
This file is part of aca-processor-simulator.

aca-processor-simulator is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

aca-processor-simulator is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with aca-processor-simulator.  If not, see <http://www.gnu.org/licenses/>.
-}
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
