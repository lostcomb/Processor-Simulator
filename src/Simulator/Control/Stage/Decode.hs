module Control.Stage.Decode
  ( decode
  ) where

import Data.Bits
import Data.Word
import Data.List
import Control.Monad.State

import Data.Stage
import Data.Processor
import Data.Registers
import Data.Instruction

decode :: (Processor p, Decode p) => [ Maybe (Word8, Word8, Word8, Word8) ] -> State p [ Maybe InstructionReg ]
decode = do d <- getDecode
            if stalled d
              then do output <- getIssInputLatches
                      return output
              else mapM decode'

decode' :: (Processor p, Decode p) => Maybe (Word8, Word8, Word8, Word8) -> State p (Maybe InstructionReg)
decode' Nothing = return Nothing
decode' (Just i) = do let inst = decodeInst i
                      cycles <- getInstructionCycles inst
                      return $ Just $ Instruction cycles inst

decodeInst :: (Word8, Word8, Word8, Word8) -> Inst Register
decodeInst (b1, b2, b3, b4) = case op_code of
  0  -> Nop
  1  -> Add (reg op1) (reg op2) (reg op3)
  2  -> Sub (reg op1) (reg op2) (reg op3)
  3  -> Mul (reg op1) (reg op2) (reg op3)
  4  -> Div (reg op1) (reg op2) (reg op3)
  5  -> And (reg op1) (reg op2) (reg op3)
  6  -> Or  (reg op1) (reg op2) (reg op3)
  7  -> Not (reg op1) (reg op2)
  8  -> Jmp           (reg op2)
  9  -> Bez           (reg op2)           imm
  10 -> Ceq (reg op1) (reg op2) (reg op3)
  11 -> Cgt (reg op1) (reg op2) (reg op3)
  12 -> Ldc (reg op1)                     imm
  13 -> Ldm (reg op1) (reg op2)
  14 -> Stm           (reg op2) (reg op3)
  15 -> Halt
  where op_code   = (b1 .&. 0xF0) `shiftR` 4
        op1       = (b1 .&. 0x0F)
        op2       = (b2 .&. 0xF0) `shiftR` 4
        op3       = (b2 .&. 0x0F)
        b3_32     = (fromIntegral b3) :: Word32
        b4_32     = (fromIntegral b4) :: Word32
        imm       = fromIntegral $ (b3_32 `shiftL` 8) .|. b4_32

reg :: Word8 -> Register
reg x = genericIndex [(minBound :: Register)..] x
