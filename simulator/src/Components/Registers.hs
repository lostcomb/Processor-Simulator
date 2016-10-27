module Components.Registers
  ( Register(..)
  , RegisterName(..)
  , pc
  ) where

data RegisterName =
    R0
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
  deriving (Show, Ord, Eq, Read)

data Register = Reg RegisterName
  deriving (Show, Eq, Read)

pc :: RegisterName
pc = R0
