{-# LANGUAGE GADTs #-}
module Simulator.Data.Instruction
  ( Inst(..)
  , Instruction(..)
  , InstructionVal
  , InstructionReg
  ) where

import Data.Int
import Simulator.Data.Registers

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
