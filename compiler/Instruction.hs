{-# LANGUAGE GADTs #-}

module Instruction
  ( Label
  , Constant
  , Register
  , Offset
  , Instruction(..)
  ) where

import Data.Either

type Label    = String
type Constant = Int
type Register = Int
type Offset   = Either Constant Label

data Instruction where
  ADD   :: Register -> Register -> Register -> Instruction
  SUB   :: Register -> Register -> Register -> Instruction
  MUL   :: Register -> Register -> Register -> Instruction
  DIV   :: Register -> Register -> Register -> Instruction
  AND   :: Register -> Register -> Register -> Instruction
  OR    :: Register -> Register -> Register -> Instruction
  XOR   :: Register -> Register -> Register -> Instruction
  NOT   :: Register -> Register -> Register -> Instruction
  JMP   :: Offset                           -> Instruction
  BEQ   :: Offset   -> Register -> Register -> Instruction
  BGT   :: Offset   -> Register -> Register -> Instruction
  BEZ   :: Offset   -> Register             -> Instruction
  LDC   :: Register -> Constant             -> Instruction
  LDM   :: Register -> Register             -> Instruction
  STM   :: Register -> Register             -> Instruction
  NOP   ::                                     Instruction
  LABEL :: Label                            -> Instruction
