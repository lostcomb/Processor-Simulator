{-# LANGUAGE GADTs #-}

module Assembly.Instruction
  ( Label
  , Constant
  , Register
  , Offset
  , Instruction(..)
  ) where

import Data.Either

type Label     = String
type Constant  = Int
type Register  = Int
type Offset    = Either Constant Label

data Instruction where
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
  -- Rd <- Ri ^ Rj
  XOR   :: Register -> Register -> Register -> Instruction
  -- Rd <- ¬Ri
  NOT   :: Register -> Register             -> Instruction
  -- PC = PC + Ri
  JMP   :: Register                         -> Instruction
  -- PC = PC + #O if Ri == Rj
  BEQ   :: Offset   -> Register -> Register -> Instruction
  -- PC = PC + #O if Ri > Rj
  BGT   :: Offset   -> Register -> Register -> Instruction
  -- PC = PC + #O if Ri == 0
  BEZ   :: Offset   -> Register             -> Instruction
  -- Rd <- #C
  LDC   :: Register -> Offset               -> Instruction
  -- Rd <- MEM[Ri]
  LDM   :: Register -> Register             -> Instruction
  -- MEM[Ri] <- Rj
  STM   :: Register -> Register             -> Instruction
  -- No operation.
  NOP   ::                                     Instruction
  -- Program marker, not to be executed by the processor.
  LABEL :: Label                            -> Instruction
  deriving (Show, Eq, Read)
