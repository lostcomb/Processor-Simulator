{-# LANGUAGE GADTs #-}
module Simulator.Data.Instruction
  ( module Simulator.Data.Instruction
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

-- |This function returns true if the specified instruction is a nop.
isNop :: Inst a -> Bool
isNop (Nop      ) = True
isNop _           = False

-- |This function returns true if the specified instruction is a add.
isAdd :: Inst a -> Bool
isAdd (Add _ _ _) = True
isAdd _           = False

-- |This function returns true if the specified instruction is a sub.
isSub :: Inst a -> Bool
isSub (Sub _ _ _) = True
isSub _           = False

-- |This function returns true if the specified instruction is a mul.
isMul :: Inst a -> Bool
isMul (Mul _ _ _) = True
isMul _           = False

-- |This function returns true if the specified instruction is a div.
isDiv :: Inst a -> Bool
isDiv (Div _ _ _) = True
isDiv _           = False

-- |This function returns true if the specified instruction is a and.
isAnd :: Inst a -> Bool
isAnd (And _ _ _) = True
isAnd _           = False

-- |This function returns true if the specified instruction is a or.
isOr  :: Inst a -> Bool
isOr  (Or  _ _ _) = True
isOr  _           = False

-- |This function returns true if the specified instruction is a not.
isNot :: Inst a -> Bool
isNot (Not _ _  ) = True
isNot _           = False

-- |This function returns true if the specified instruction is a jmp.
isJmp :: Inst a -> Bool
isJmp (Jmp _    ) = True
isJmp _           = False

-- |This function returns true if the specified instruction is a bez.
isBez :: Inst a -> Bool
isBez (Bez _ _  ) = True
isBez _           = False

-- |This function returns true if the specified instruction is a ceq.
isCeq :: Inst a -> Bool
isCeq (Ceq _ _ _) = True
isCeq _           = False

-- |This function returns true if the specified instruction is a cgt.
isCgt :: Inst a -> Bool
isCgt (Cgt _ _ _) = True
isCgt _           = False

-- |This function returns true if the specified instruction is a ldc.
isLdc :: Inst a -> Bool
isLdc (Ldc _ _  ) = True
isLdc _           = False

-- |This function returns true if the specified instruction is a ldm.
isLdm :: Inst a -> Bool
isLdm (Ldm _ _  ) = True
isLdm _           = False

-- |This function returns true if the specified instruction is a stm.
isStm :: Inst a -> Bool
isStm (Stm _ _  ) = True
isStm _           = False

-- |This function returns true if the specified instruction is a halt.
isHalt :: Inst a -> Bool
isHalt (Halt    ) = True
isHalt _          = False
