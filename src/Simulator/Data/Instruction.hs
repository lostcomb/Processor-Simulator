{-# LANGUAGE GADTs #-}
module Simulator.Data.Instruction
  ( module Simulator.Data.Instruction
  ) where

import Data.Int
import Data.Word
import Simulator.Data.Registers

-- |This defines the length of an instruction in bytes.
instLength :: (Integral a) => a
instLength = 4

-- |This defines the type for Instructions whose registers have been swapped
--  out for their values.
type InstructionVal = Instruction Int32 Int
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

-- |This function returns true if the specified register is in the specified
--  list of registers.
usesRegister :: Maybe (Register, Int32, Bool) -> [ Register ] -> Bool
usesRegister (Nothing       ) _  = False
usesRegister (Just (r, _, _)) rs = elem r rs

-- |This function returns the list of registers the specified instruction uses
--  as operands.
instOperands :: Inst Register -> [ Register ]
instOperands i = case i of
  (Nop        ) -> []
  (Add _ ri rj) -> [ ri, rj ]
  (Sub _ ri rj) -> [ ri, rj ]
  (Mul _ ri rj) -> [ ri, rj ]
  (Div _ ri rj) -> [ ri, rj ]
  (And _ ri rj) -> [ ri, rj ]
  (Or  _ ri rj) -> [ ri, rj ]
  (Not _ ri   ) -> [ ri ]
  (Jmp   ri   ) -> [ ri ]
  (Bez   ri  _) -> [ ri ]
  (Ceq _ ri rj) -> [ ri, rj ]
  (Cgt _ ri rj) -> [ ri, rj ]
  (Ldc _     _) -> []
  (Ldm _ ri   ) -> [ ri ]
  (Stm   ri rj) -> [ ri, rj ]
  (Halt       ) -> []

-- |This data type adds an extra parameter to the Inst type. This can be used
--  to make instructions take more than one cycle.
data Instruction t c = Instruction c (Inst t) Control
  deriving (Show, Read)

-- |This data type defines the control type of an instruction. If the
--  instruction isn't a branch instruction, then its control is NA. If the
--  instruction is a branch instruction then its control type is either taken
--  or not taken.
data Control = NA
             | Taken    Word32 Word32
             | NotTaken Word32 Word32
             deriving (Show, Eq, Read)

-- |This function returns true if the specified control is an NA.
isNA :: Control -> Bool
isNA       (NA          ) = True
isNA       _              = False

-- |This function returns true if the specified control is a Taken.
isTaken :: Control -> Bool
isTaken    (Taken    _ _) = True
isTaken    _              = False

-- |This function returns true if the specified control is a NotTaken.
isNotTaken :: Control -> Bool
isNotTaken (NotTaken _ _) = True
isNotTaken _              = False

-- |This function returns the target of the control.
getControlTarget :: Control -> Word32
getControlTarget (NA          ) = undefined
getControlTarget (Taken    _ t) = t
getControlTarget (NotTaken _ t) = t

-- |This function returns the pc of the control.
getControlPC :: Control -> Word32
getControlPC (NA           ) = undefined
getControlPC (Taken    pc _) = pc
getControlPC (NotTaken pc _) = pc

-- |This instance allows fmapping over the extra value in the Instruction type.
instance Functor (Instruction t) where
  fmap f (Instruction c i co) = (Instruction (f c) i co)

-- |This instance allows us to order instructions.
instance (Eq c) => Eq (Instruction t c) where
  (Instruction a _ _) == (Instruction b _ _) = a == b

-- |This instance allows us to order instructions.
instance (Ord c) => Ord (Instruction t c) where
  compare (Instruction a _ _) (Instruction b _ _) = compare a b

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
defaultCycles (Ldm _ _  ) = 20
defaultCycles (Stm   _ _) = 20
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
