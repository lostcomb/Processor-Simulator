module Components.Processor
  ( Processor
  , newProcessor
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
import Control.Monad.State

data Processor = Processor
  { instructions  :: [ Word8 ]
  , memory        :: Map Word32 Int32
  , registers     :: RegisterFile
  , cycles        :: Int
  , executedInsts :: Int
  , halted        :: Bool
  }

newProcessor :: [ Word8 ] -> Processor
newProcessor instructions = Processor
  { instructions  = instructions
  , memory        = Map.empty
  , registers     = emptyRegisterFile
  , cycles        = 0
  , executedInsts = 0
  , halted        = False
  }

getRegisterFile :: State Processor RegisterFile
getRegisterFile = state $ \s -> (registers s, s)

getReg :: RegisterName -> State Processor Int32
getReg r = state $ \s -> (getRegister r (registers s), s)

setReg :: RegisterName -> Int32 -> State Processor ()
setReg r v = state $ \s -> ((), s { registers = setRegister r v (registers s) })

getInstruction :: Word32 -> State Processor Word8
getInstruction i = state $ \s -> (genericIndex (instructions s) i, s)

getMemoryContents :: State Processor [ (Word32, Int32) ]
getMemoryContents = state $ \s -> (Map.toList (memory s), s)

getMemory :: Word32 -> State Processor Int32
getMemory i = state $ \s -> (Map.findWithDefault 0 i (memory s), s)

setMemory :: Word32 -> Int32 -> State Processor ()
setMemory i v = state $ \s -> ((), s { memory = Map.insert i v (memory s) })

updateCycles :: Int -> State Processor ()
updateCycles o = state $ \s -> ((), s { cycles = (cycles s) + o })

getCycles :: State Processor Int
getCycles = state $ \s -> (cycles s, s)

incExecutedInsts :: State Processor ()
incExecutedInsts = state $ \s -> ((), s { executedInsts = (executedInsts s) + 1 })

getExecutedInsts :: State Processor Int
getExecutedInsts = state $ \s -> (executedInsts s, s)

isHalted :: State Processor Bool
isHalted = state $ \s -> (halted s, s)

haltExecution :: State Processor ()
haltExecution = state $ \s -> ((), s { halted = True })
