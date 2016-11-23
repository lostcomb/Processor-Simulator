{-# LANGUAGE RankNTypes #-}
module Simulator.Data.Registers
  ( module Simulator.Data.Registers
  ) where

import Data.Int
import Control.Lens

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

-- |This defines the flags a register in the register file can have. It can be
--  used to indicate a registers value is in the process of being updated.
data Flag = Clean
          | Dirty
  deriving (Show, Eq, Read)

-- |This defines a new register file using all of the registers defined by
--  Register.
newRegFile :: RegisterFile
newRegFile = zip [(minBound :: Register)..] (repeat (0, Clean))

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

-- |This function returns the value associated with the key @r@ applied to @f@.
--  Calls 'error' if the key @r@ is not in the association list.
searchWith :: (Eq a) => (b -> c) -> a -> [ (a, b) ] -> c
searchWith f r []           = error "searchWith: element not part of the specified list."
searchWith f r ((x, y) : l)
  | r == x                  = f y
  | otherwise               = searchWith f r l

-- |This function returns the new association list with the value associated
--  with the key @r@ applied to the function @f@. Calls 'error' if the key
--  @r@ is not in the association list.
updateWith :: (Eq a) => (b -> b) -> a -> [ (a, b) ] -> [ (a, b) ]
updateWith f r []           = error "updateWith: element not part of the specified list."
updateWith f r ((x, y) : l)
  | r == x                  = (x, f y) : l
  | otherwise               = (x,   y) : updateWith f r l
