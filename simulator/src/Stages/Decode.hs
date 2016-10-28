module Stages.Decode
  ( decode
  ) where

import Data.Word
import Data.Bits
import Components.Registers
import Components.Instructions
import Components.ProcessorState

decode :: (Word8, Word8, Word8, Word8) -> ProcessorState
       -> (Instruction Int, ProcessorState)
decode b s = (decodeInstruction b, updateCycles 1 s)

decodeInstruction :: (Word8, Word8, Word8, Word8) -> Instruction Int
decodeInstruction (b1, b2, b3, b4) = case op_code of
  0  -> Nop op_length
  1  -> Add op_length (reg op1) (reg op2) (reg op3)
  2  -> Sub op_length (reg op1) (reg op2) (reg op3)
  3  -> Mul op_length (reg op1) (reg op2) (reg op3)
  4  -> Div op_length (reg op1) (reg op2) (reg op3)
  5  -> And op_length (reg op1) (reg op2) (reg op3)
  6  -> Or  op_length (reg op1) (reg op2) (reg op3)
  7  -> Not op_length (reg op1) (reg op2)
  8  -> Jmp op_length           (reg op2)
  9  -> Bez op_length           (reg op2)           imm
  10 -> Ceq op_length (reg op1) (reg op2) (reg op3)
  11 -> Cgt op_length (reg op1) (reg op2) (reg op3)
  12 -> Ldc op_length (reg op1)                     imm
  13 -> Ldm op_length (reg op1) (reg op2)
  14 -> Stm op_length           (reg op2) (reg op3)
  15 -> Halt
  where op_code   = (b1 .&. 0xF0) `shiftR` 4
        op1       = (b1 .&. 0x0F)
        op2       = (b2 .&. 0xF0) `shiftR` 4
        op3       = (b2 .&. 0x0F)
        b3_32     = (fromIntegral b3) :: Word32
        b4_32     = (fromIntegral b4) :: Word32
        imm       = (b3_32 `shiftL` 8) .|. b4_32
        op_length = defaultCycles op_code

defaultCycles :: Word8 -> Int
defaultCycles op_code = 1

registers :: [ (Word8, Register) ]
registers = [ (0 , Reg R0 ), (1 , Reg R1 ), (2 , Reg R2 ), (3 , Reg R3 )
            , (4 , Reg R4 ), (5 , Reg R5 ), (6 , Reg R6 ), (7 , Reg R7 )
            , (8 , Reg R8 ), (9 , Reg R9 ), (10, Reg R10), (11, Reg R11)
            , (12, Reg R12), (13, Reg R13), (14, Reg R14), (15, Reg R15)
            ]

reg :: Word8 -> Register
reg x = case lookup x registers of
  (Just reg) -> reg
  (Nothing ) -> undefined
