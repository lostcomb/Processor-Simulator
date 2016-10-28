module Components.RegisterFile
  ( RegisterFile
  , emptyRegisterFile
  , getRegisters
  , getRegister
  , setRegister
  ) where

import Data.Word
import Components.Registers
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map

type RegisterFile = Map RegisterName Word32

emptyRegisterFile :: RegisterFile
emptyRegisterFile = Map.fromList [ (R0 , 0), (R1 , 0), (R2 , 0)
                                 , (R3 , 0), (R4 , 0), (R5 , 0)
                                 , (R6 , 0), (R7 , 0), (R8 , 0)
                                 , (R9 , 0), (R10, 0), (R11, 0)
                                 , (R12, 0), (R13, 0), (R14, 0)
                                 , (R15, 0)
                                 ]

getRegisters :: RegisterFile -> [ (RegisterName, Word32) ]
getRegisters = Map.toList

getRegister :: RegisterName -> RegisterFile -> Word32
getRegister = Map.findWithDefault 0

setRegister :: RegisterName -> Word32 -> RegisterFile -> RegisterFile
setRegister = Map.insert
