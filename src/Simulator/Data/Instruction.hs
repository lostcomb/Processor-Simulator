{-
This file is part of aca-processor-simulator.

aca-processor-simulator is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

aca-processor-simulator is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with aca-processor-simulator.  If not, see <http://www.gnu.org/licenses/>.
-}
{-# LANGUAGE GADTs #-}
module Simulator.Data.Instruction
  ( module Simulator.Data.Instruction
  ) where

import Data.Int
import Data.Word
import Simulator.Data.BTAC
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

-- |This data type distinguished between a control instruction, store instruction
--  and arithmetic / logic / load instructions.
data InstType = Control
              |  Other
              deriving (Show, Eq, Read)

-- |This function returns the instruction type for the specified instruction.
instType :: Inst Register -> InstType
instType i
  | isJmp i || isBez i = Control
  | otherwise          = Other

-- |This function returns true if the specified register is in the specified
--  list of registers.
usesRegister :: Maybe (Register, Int32) -> [ Register ] -> Bool
usesRegister (Nothing    ) _  = False
usesRegister (Just (r, _)) rs = elem r rs

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

-- |This function returns the destination register written to by the specified
--  instruction, or nothing it if doesn't use one.
instDestination :: Inst Register -> Maybe Register
instDestination i = case i of
  (Nop       ) -> Nothing
  (Add rd _ _) -> Just rd
  (Sub rd _ _) -> Just rd
  (Mul rd _ _) -> Just rd
  (Div rd _ _) -> Just rd
  (And rd _ _) -> Just rd
  (Or  rd _ _) -> Just rd
  (Not rd _  ) -> Just rd
  (Jmp    _  ) -> Just pc
  (Bez    _ _) -> Just pc
  (Ceq rd _ _) -> Just rd
  (Cgt rd _ _) -> Just rd
  (Ldc rd   _) -> Just rd
  (Ldm rd _  ) -> Just rd
  (Stm    _ _) -> Nothing
  (Halt      ) -> Nothing

-- |This data type adds an extra parameter to the Inst type. This can be used
--  to make instructions take more than one cycle.
data Instruction t c = Instruction c (Inst t) Control
  deriving (Show, Read)

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
defaultCycles (Ldm _ _  ) = 3
defaultCycles (Stm   _ _) = 3
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
