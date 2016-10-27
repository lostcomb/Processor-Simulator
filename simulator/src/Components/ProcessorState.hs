module Components.ProcessorState
  ( ProcessorState
  , newProcessorState
  , getReg
  , setReg
  , getInstruction
  , getMemory
  , setMemory
  , updateCycles
  , incExecutedInsts
  , isHalted
  , haltExecution
  ) where

import Data.List
import Data.Word
import Data.Map (Map)
import qualified Data.Map as Map
import Components.Registers
import Components.RegisterFile

data ProcessorState = ProcessorState
  { instructions  :: [ Word32 ]
  , memory        :: Map Word32 Word32
  , registers     :: RegisterFile
  , cycles        :: Int
  , executedInsts :: Int
  , halted        :: Bool
  }

newProcessorState :: [ Word32 ] -> ProcessorState
newProcessorState instructions = ProcessorState
  { instructions  = instructions
  , memory        = Map.empty
  , registers     = emptyRegisterFile
  , cycles        = 0
  , executedInsts = 0
  , halted        = False
  }

getReg :: RegisterName -> ProcessorState -> Word32
getReg r s = getRegister r (registers s)

setReg :: RegisterName -> Word32 -> ProcessorState -> ProcessorState
setReg r v s = s { registers = setRegister r v (registers s) }

getInstruction :: Word32 -> ProcessorState -> Word32
getInstruction i s = genericIndex (instructions s) i

getMemory :: Word32 -> ProcessorState -> Word32
getMemory i s = Map.findWithDefault 0 i (memory s)

setMemory :: Word32 -> Word32 -> ProcessorState -> ProcessorState
setMemory i v s = s { memory = Map.insert i v (memory s) }

updateCycles :: Int -> ProcessorState -> ProcessorState
updateCycles o s = s { cycles = (cycles s) + o }

incExecutedInsts :: ProcessorState -> ProcessorState
incExecutedInsts s = s { executedInsts = (executedInsts s) + 1 }

isHalted :: ProcessorState -> Bool
isHalted s = halted s

haltExecution :: ProcessorState -> ProcessorState
haltExecution s = s { halted = True }
