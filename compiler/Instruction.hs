{-# LANGUAGE GADTs #-}

module Instruction
  ( Label
  , Constant
  , Register
  , Offset
  , Instruction(..)
  ) where

import Data.Word
import Data.Either

type Label    = String
type Constant = Word32
type Register = Word32
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
