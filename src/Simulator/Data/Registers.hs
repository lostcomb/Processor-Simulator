{-# LANGUAGE RankNTypes #-}
module Simulator.Data.Registers
  ( module Simulator.Data.Registers
  ) where

import Data.Int
import Control.Lens

-- |This type defines a register file for the processor.
type RegisterFile = [ (Register, (Int32, Maybe Flag)) ]

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
data Flag = Invalid--TODO
  deriving (Show, Eq, Read)

newRegFile :: RegisterFile
newRegFile = zip [(minBound :: Register)..] (repeat (0, Nothing))

regVal :: Register -> Lens' RegisterFile Int32
regVal r = lens (\rf   -> searchWith (\(v, _) -> v     ) r rf)
                (\rf v -> updateWith (\(_, f) -> (v, f)) r rf)

regFlag :: Register -> Lens' RegisterFile (Maybe Flag)
regFlag r = lens (\rf   -> searchWith (\(_, f) -> f     ) r rf)
                 (\rf f -> updateWith (\(v, _) -> (v, f)) r rf)

searchWith :: (Eq a) => (b -> c) -> a -> [ (a, b) ] -> c
searchWith f r []           = error "searchWith: element not part of the specified list."
searchWith f r ((x, y) : l)
  | r == x                  = f y
  | otherwise               = searchWith f r l

updateWith :: (Eq a) => (b -> b) -> a -> [ (a, b) ] -> [ (a, b) ]
updateWith f r []           = error "updateWith: element not part of the specified list."
updateWith f r ((x, y) : l)
  | r == x                  = (x, f y) : l
  | otherwise               = (x,   y) : updateWith f r l
