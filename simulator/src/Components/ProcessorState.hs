module Components.ProcessorState
  ( ProcessorState
  , newProcessorState
  , getRegisterFile
  , getReg
  , setReg
  , getInstruction
  , getMemoryContents
  , getMemory
  , setMemory
  , updateCycles
  , getCycles
  , incExecutedInsts
  , getExecutedInsts
  , isHalted
  , haltExecution
  ) where

import Data.Int
import Data.Word
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Components.Registers
import Components.RegisterFile

data ProcessorState = ProcessorState
  { instructions  :: [ Word8 ]
  , memory        :: Map Word32 Int32
  , registers     :: RegisterFile
  , cycles        :: Int
  , executedInsts :: Int
  , halted        :: Bool
  }

newProcessorState :: [ Word8 ] -> ProcessorState
newProcessorState instructions = ProcessorState
  { instructions  = instructions
  , memory        = Map.empty
  , registers     = emptyRegisterFile
  , cycles        = 0
  , executedInsts = 0
  , halted        = False
  }

getRegisterFile :: ProcessorState -> RegisterFile
getRegisterFile s = registers s

getReg :: RegisterName -> ProcessorState -> Int32
getReg r s = getRegister r (registers s)

setReg :: RegisterName -> Int32 -> ProcessorState -> ProcessorState
setReg r v s = s { registers = setRegister r v (registers s) }

getInstruction :: Word32 -> ProcessorState -> Word8
getInstruction i s = genericIndex (instructions s) i

getMemoryContents :: ProcessorState -> [ (Word32, Int32) ]
getMemoryContents s = Map.toList (memory s)

getMemory :: Word32 -> ProcessorState -> Int32
getMemory i s = Map.findWithDefault 0 i (memory s)

setMemory :: Word32 -> Int32 -> ProcessorState -> ProcessorState
setMemory i v s = s { memory = Map.insert i v (memory s) }

updateCycles :: Int -> ProcessorState -> ProcessorState
updateCycles o s = s { cycles = (cycles s) + o }

getCycles :: ProcessorState -> Int
getCycles s = cycles s

incExecutedInsts :: ProcessorState -> ProcessorState
incExecutedInsts s = s { executedInsts = (executedInsts s) + 1 }

getExecutedInsts :: ProcessorState -> Int
getExecutedInsts s = executedInsts s

isHalted :: ProcessorState -> Bool
isHalted s = halted s

haltExecution :: ProcessorState -> ProcessorState
haltExecution s = s { halted = True }
