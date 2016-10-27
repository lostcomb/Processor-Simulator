module Stages.Decode
  ( decode
  ) where

import Data.Word
import Data.Bits
import Components.Registers
import Components.Instructions
import Components.ProcessorState

decode :: Word32 -> ProcessorState -> (Instruction Int, ProcessorState)
decode b s = (decodeInstruction b, s)

decodeInstruction :: Word32 -> Instruction Int
decodeInstruction b = case op_code of
  0  -> Nop op_length
  1  -> Add op_length (reg operand1) (reg operand2) (reg operand3)
  2  -> Sub op_length (reg operand1) (reg operand2) (reg operand3)
  3  -> Mul op_length (reg operand1) (reg operand2) (reg operand3)
  4  -> Div op_length (reg operand1) (reg operand2) (reg operand3)
  5  -> And op_length (reg operand1) (reg operand2) (reg operand3)
  6  -> Or  op_length (reg operand1) (reg operand2) (reg operand3)
  7  -> Not op_length (reg operand1) (reg operand2)
  8  -> Jmp op_length                (reg operand2)
  9  -> Bez op_length                (reg operand2)                immediate
  10 -> Ceq op_length (reg operand1) (reg operand2) (reg operand3)
  11 -> Cgt op_length (reg operand1) (reg operand2) (reg operand3)
  12 -> Ldc op_length (reg operand1)                               immediate
  13 -> Ldm op_length (reg operand1) (reg operand2)
  14 -> Stm op_length                (reg operand2) (reg operand3)
  15 -> Halt
  where op_code   = (b .&. 0xF0000000) `shiftR` 28
        operand1  = (b .&. 0x0F000000) `shiftR` 24
        operand2  = (b .&. 0x00F00000) `shiftR` 20
        operand3  = (b .&. 0x000F0000) `shiftR` 16
        immediate = (b .&. 0x0000FFFF)
        op_length = defaultCycles op_code

defaultCycles :: Word32 -> Int
defaultCycles op_code = 1

reg :: Word32 -> Register
reg 0  = Reg R0
reg 1  = Reg R1
reg 2  = Reg R2
reg 3  = Reg R3
reg 4  = Reg R4
reg 5  = Reg R5
reg 6  = Reg R6
reg 7  = Reg R7
reg 8  = Reg R8
reg 9  = Reg R9
reg 10 = Reg R10
reg 11 = Reg R11
reg 12 = Reg R12
reg 13 = Reg R13
reg 14 = Reg R14
reg 15 = Reg R15
reg _ = undefined
