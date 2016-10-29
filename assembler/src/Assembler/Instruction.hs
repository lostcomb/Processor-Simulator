{-# LANGUAGE GADTs #-}
-- |This module defines the simple instruction set.

module Assembler.Instruction
  ( Label
  , Constant
  , Register
  , Offset
  , Instruction(..)
  ) where

import Data.Int
import Data.Word
import Data.Either

type Label     = String
type Constant  = Int16
type Register  = Word8
type Offset    = Either Constant Label

data Instruction where
  -- No operation.
  NOP   ::                                     Instruction
  -- Rd <- Ri + Rj.
  ADD   :: Register -> Register -> Register -> Instruction
  -- Rd <- Ri - Rj
  SUB   :: Register -> Register -> Register -> Instruction
  -- Rd <- Ri * Rj
  MUL   :: Register -> Register -> Register -> Instruction
  -- Rd <- Ri / Rj
  DIV   :: Register -> Register -> Register -> Instruction
  -- Rd <- Ri & Rj
  AND   :: Register -> Register -> Register -> Instruction
  -- Rd <- Ri | Rj
  OR    :: Register -> Register -> Register -> Instruction
  -- Rd <- ¬Ri
  NOT   :: Register -> Register             -> Instruction
  -- PC = Ri
  JMP   :: Register                         -> Instruction
  -- PC = #O if Ri == 0
  BEZ   :: Register -> Offset               -> Instruction
  -- Rd <- 1 if Ri == Rj, Rd <- 0 otherwise
  CEQ   :: Register -> Register -> Register -> Instruction
  -- Rd <- 1 if Ri <  Rj, Rd <- 0 otherwise
  CGT   :: Register -> Register -> Register -> Instruction
  -- Rd <- #C
  LDC   :: Register -> Offset               -> Instruction
  -- Rd <- MEM[Ri]
  LDM   :: Register -> Register             -> Instruction
  -- MEM[Ri] <- Rj
  STM   :: Register -> Register             -> Instruction
  -- Halts execution.
  HALT  ::                                     Instruction
  -- Program marker, not to be executed by the processor.
  LABEL :: Label                            -> Instruction
  deriving (Show, Eq, Read)
