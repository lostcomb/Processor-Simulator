module Simulator.Data.Registers
  ( Register(..)
  , pc
  , RegisterFile
  , Flag(..)
  , newRegFile
  ) where

import Data.Int

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

type RegisterFile = [ (Register, Int32, Maybe Flag) ]

data Flag = Invalid--TODO

newRegFile :: RegisterFile
newRegFile = zip3 [(minBound :: Register)..] (repeat 0) (repeat Nothing)
