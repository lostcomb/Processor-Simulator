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
-- |This module defines the simple instruction set.
module Compiler.Instruction
  ( Label
  , Constant
  , Register
  , Offset
  , Instruction(..)
  , pc
  , sp
  , gprb
  ) where

import Data.Either

type Label     = String
type Constant  = Int
type Register  = Int
type Offset    = Either Constant Label

-- |These constants define the program counter (pc), the stack pointer (sp)
--  and the base general purpose register (gprb).
pc, sp, gprb :: Register
pc = 0
sp = 1
gprb = 2

data Instruction where
  -- Rd <- Ri + Rj.
  ADD   :: Register -> Register -> Register           -> Instruction
  -- Rd <- Ri - Rj
  SUB   :: Register -> Register -> Register           -> Instruction
  -- Rd <- Ri * Rj
  MUL   :: Register -> Register -> Register           -> Instruction
  -- Rd <- Ri / Rj
  DIV   :: Register -> Register -> Register           -> Instruction
  -- Rd <- Ri & Rj
  AND   :: Register -> Register -> Register           -> Instruction
  -- Rd <- Ri | Rj
  OR    :: Register -> Register -> Register           -> Instruction
  -- Rd <- ¬Ri
  NOT   :: Register -> Register                       -> Instruction
  -- PC = Ri
  JMP   ::             Register                       -> Instruction
  -- PC = #O if Ri == 0
  BEZ   ::             Register             -> Offset -> Instruction
  -- Rd <- 1 if Ri == Rj, Rd <- 0 otherwise
  CEQ   :: Register -> Register -> Register           -> Instruction
  -- Rd <- 1 if Ri <  Rj, Rd <- 0 otherwise
  CGT   :: Register -> Register -> Register           -> Instruction
  -- Rd <- #C
  LDC   :: Register                         -> Offset -> Instruction
  -- Rd <- MEM[Ri]
  LDM   :: Register -> Register                       -> Instruction
  -- MEM[Ri] <- Rj
  STM   ::             Register -> Register           -> Instruction
  -- No operation.
  NOP   ::                                               Instruction
  -- Halt execution.
  HALT  ::                                               Instruction
  -- Program marker, not to be executed by the processor.
  LABEL :: Label                                      -> Instruction
  deriving (Eq, Read, Ord)

instance Show Instruction where
  show (ADD rd ri rj          ) = "ADD r" ++ show rd ++ " r" ++ show ri ++ " r" ++ show rj
  show (SUB rd ri rj          ) = "SUB r" ++ show rd ++ " r" ++ show ri ++ " r" ++ show rj
  show (MUL rd ri rj          ) = "MUL r" ++ show rd ++ " r" ++ show ri ++ " r" ++ show rj
  show (DIV rd ri rj          ) = "DIV r" ++ show rd ++ " r" ++ show ri ++ " r" ++ show rj
  show (AND rd ri rj          ) = "AND r" ++ show rd ++ " r" ++ show ri ++ " r" ++ show rj
  show (OR  rd ri rj          ) = "OR r" ++ show rd ++ " r" ++ show ri ++ " r" ++ show rj
  show (NOT rd ri             ) = "NOT r" ++ show rd ++ " r" ++ show ri
  show (JMP    ri             ) = "JMP r" ++ show ri
  show (BEZ    ri    (Left  c)) = "BEZ r" ++ show ri ++ " #" ++ show c
  show (BEZ    ri    (Right l)) = "BEZ r" ++ show ri ++ " :" ++ l
  show (CEQ rd ri rj          ) = "CEQ r" ++ show rd ++ " r" ++ show ri ++ " r" ++ show rj
  show (CGT rd ri rj          ) = "CGT r" ++ show rd ++ " r" ++ show ri ++ " r" ++ show rj
  show (LDC rd       (Left  c)) = "LDC r" ++ show rd ++ " #" ++ show c
  show (LDC rd       (Right l)) = "LDC r" ++ show rd ++ " :" ++ l
  show (LDM rd ri             ) = "LDM r" ++ show rd ++ " r" ++ show ri
  show (STM    ri rj          ) = "STM r" ++ show ri ++ " r" ++ show rj
  show (NOP                   ) = "NOP"
  show (HALT                  ) = "HALT"
  show (LABEL l               ) = ":" ++ l
