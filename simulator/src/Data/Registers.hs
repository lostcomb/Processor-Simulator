module Data.Registers
  ( Register(..)
  , RegisterFile_T
  , RegisterFile(..)
  , pc
  , newRegFile
  ) where

import Data.Int
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State

-- |This data type defines an enum which enumerates all possible registers.
data Register
  = R0
  | R1
  | R2
  | R3
  | R4
  | R5
  | R6
  | R7
  | R8
  | R9
  | R10
  | R11
  | R12
  | R13
  | R14
  | R15
  deriving (Show, Ord, Eq, Bounded, Enum, Read)

-- |This defines the register used for the program counter.
pc :: Register
pc = R0

type RegisterFile_T = Map Register Int32

newRegFile :: RegisterFile_T
newRegFile = Map.fromList $ zip [(minBound :: Register)..] $ repeat 0

class RegisterFile r where
  getRegs :: State r [ (Register, Int32) ]
  getReg  :: Register -> State r Int32
  setReg  :: Register -> Int32 -> State r ()
