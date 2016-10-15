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
  -- Rd <- ¬Ri
  NOT   :: Register -> Register             -> Instruction
  -- PC = Ri
  JMP   :: Register                         -> Instruction
  -- PC = #O if Ri == 0
  BEZ   :: Offset   -> Register             -> Instruction
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
  -- No operation.
  NOP   ::                                     Instruction
  -- Program marker, not to be executed by the processor.
  LABEL :: Label                            -> Instruction
  deriving (Eq)

instance Show Instruction where
  show i = case i of
    (ADD rd ri rj       ) -> "ADD " ++ showReg rd ++ " " ++ showReg ri ++ " " ++ showReg rj
    (SUB rd ri rj       ) -> "SUB " ++ showReg rd ++ " " ++ showReg ri ++ " " ++ showReg rj
    (MUL rd ri rj       ) -> "MUL " ++ showReg rd ++ " " ++ showReg ri ++ " " ++ showReg rj
    (DIV rd ri rj       ) -> "DIV " ++ showReg rd ++ " " ++ showReg ri ++ " " ++ showReg rj

    (AND rd ri rj       ) -> "AND " ++ showReg rd ++ " " ++ showReg ri ++ " " ++ showReg rj
    (OR  rd ri rj       ) -> "OR "  ++ showReg rd ++ " " ++ showReg ri ++ " " ++ showReg rj
    (NOT rd ri          ) -> "NOT " ++ showReg rd ++ " " ++ showReg ri

    (JMP ri             ) -> "JMP " ++ showReg ri
    (BEZ (Left  c) ri   ) -> "BEZ " ++ showConst c ++ " " ++ showReg ri
    (BEZ (Right l) ri   ) -> "BEZ " ++ showLabel l ++ " " ++ showReg ri

    (CEQ rd ri rj       ) -> "CEQ " ++ showReg rd ++ " " ++ showReg ri ++ " " ++ showReg rj
    (CGT rd ri rj       ) -> "CGT " ++ showReg rd ++ " " ++ showReg ri ++ " " ++ showReg rj

    (LDC rd (Left  c)   ) -> "LDC " ++ showReg rd ++ " " ++ showConst c
    (LDC rd (Right l)   ) -> "LDC " ++ showReg rd ++ " " ++ showLabel l
    (LDM rd ri          ) -> "LDM " ++ showReg rd ++ " " ++ showReg ri
    (STM ri rj          ) -> "STM " ++ showReg ri ++ " " ++ showReg rj

    (NOP                ) -> "NOP"

    (LABEL l            ) -> showLabel l
    where showReg   r = "r" ++ show r
          showLabel l = ":" ++ l
          showConst c = "#" ++ show c
