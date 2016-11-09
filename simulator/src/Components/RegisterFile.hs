module Components.RegisterFile
  ( RegisterFile
  , emptyRegFile
  , getRegs
  , getReg
  , setReg
  ) where

import Data.Int
import Data.Map (Map)
import qualified Data.Map as Map
import Components.Registers

type RegisterFile = Map Register Int32

emptyRegFile :: RegisterFile
emptyRegFile = Map.fromList $ zip [(minBound :: Register)..] $ repeat 0

getRegs :: RegisterFile -> [ (Register, Int32) ]
getRegs = Map.toList

getReg :: Register -> RegisterFile -> Int32
getReg = Map.findWithDefault 0

setReg :: Register -> Int32 -> RegisterFile -> RegisterFile
setReg = Map.insert
