{-# LANGUAGE GADTs #-}
module Simulator.Data.Instruction
  ( Inst(..)
  , Instruction(..)
  , InstructionVal
  , InstructionReg
  , instLength
  , defaultCycles
  ) where

import Data.Int
import Simulator.Data.Registers

-- |This defines the length of an instruction in bytes.
instLength :: (Integral a) => a
instLength = 4

-- |This defines the type for Instructions whose registers have been swapped
--  out for their values.
type InstructionVal = Instruction Int32    Int
-- |This defines the type for Instructions whose registers are symbolic values.
type InstructionReg = Instruction Register Int

-- |This data type defines the instructions this processor supports.
data Inst t where
  Nop  ::                                Inst t
  Add  :: Register -> t -> t          -> Inst t
  Sub  :: Register -> t -> t          -> Inst t
  Mul  :: Register -> t -> t          -> Inst t
  Div  :: Register -> t -> t          -> Inst t
  And  :: Register -> t -> t          -> Inst t
  Or   :: Register -> t -> t          -> Inst t
  Not  :: Register -> t               -> Inst t
  Jmp  ::             t               -> Inst t
  Bez  ::             t      -> Int16 -> Inst t
  Ceq  :: Register -> t -> t          -> Inst t
  Cgt  :: Register -> t -> t          -> Inst t
  Ldc  :: Register           -> Int16 -> Inst t
  Ldm  :: Register -> t               -> Inst t
  Stm  ::             t -> t          -> Inst t
  Halt ::                                Inst t
  deriving (Show, Eq, Read)

-- |This data type adds an extra parameter to the Inst type. This can be used
--  to make instructions take more than one cycle.
data Instruction t c = Instruction c (Inst t)
  deriving (Show, Eq, Read)

-- |This instance allows mapping over the extra value in the Instruction type.
instance Functor (Instruction t) where
  fmap f (Instruction c i) = (Instruction (f c) i)

-- |This function maps each intruction to a number of cycles it takes to complete.
--  Instruction timings taken from here: http://www.agner.org/optimize/instruction_tables.pdf
defaultCycles :: Inst a -> Int
defaultCycles (Nop      ) = 1
defaultCycles (Add _ _ _) = 1
defaultCycles (Sub _ _ _) = 1
defaultCycles (Mul _ _ _) = 3
defaultCycles (Div _ _ _) = 11
defaultCycles (And _ _ _) = 1
defaultCycles (Or  _ _ _) = 1
defaultCycles (Not _ _  ) = 1
defaultCycles (Jmp   _  ) = 1
defaultCycles (Bez   _ _) = 1
defaultCycles (Ceq _ _ _) = 1
defaultCycles (Cgt _ _ _) = 1
defaultCycles (Ldc _   _) = 1
defaultCycles (Ldm _ _  ) = 270
defaultCycles (Stm   _ _) = 270
defaultCycles (Halt     ) = 1
