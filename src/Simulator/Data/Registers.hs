{-# LANGUAGE RankNTypes #-}
module Simulator.Data.Registers
  ( module Simulator.Data.Registers
  ) where

import Data.Int
import Control.Lens

import Simulator.Data.Association

-- |This type defines a flag to determine whether a register is clean or dirty.
--  Flag == 0 means clean, Flag > 0 means dirty, Flag < 0 is undefined.
type Flag = Int
-- |This type defines a register file for the processor.
type RegisterFile = [ (Register, (Int32, Flag)) ]

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

-- |This defines a clean register flag.
clean :: Flag
clean = 0

-- |This function returns true if the specified register flag @f@ represents a
--  clean register.
isClean :: Flag -> Bool
isClean f = f == clean

-- |This function returns true if the specified register flag @f@ represents a
--  dirty register.
isDirty :: Flag -> Bool
isDirty f = f /= clean

-- |This defines a new register file using all of the registers defined by
--  Register.
newRegFile :: RegisterFile
newRegFile = zip [(minBound :: Register)..] (repeat (0, clean))

-- |This lens provides a getter and setter for the register values in the
--  register file.
regVal :: Register -> Lens' RegisterFile Int32
regVal r = lens (\rf   -> searchWith (\(v, _) -> v     ) r rf)
                (\rf v -> updateWith (\(_, f) -> (v, f)) r rf)

-- |This lens provides a getter and setter for the register flags in the
--  register file.
regFlag :: Register -> Lens' RegisterFile Flag
regFlag r = lens (\rf   -> searchWith (\(_, f) -> f     ) r rf)
                 (\rf f -> updateWith (\(v, _) -> (v, f)) r rf)
