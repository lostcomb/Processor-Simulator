module Control.Stage.Execute
  ( execute
  ) where

import Data.Int
import Data.Bits
import Control.Monad.State

import Data.Stage
import Data.Registers
import Data.Processor
import Data.Instruction

execute :: (Processor p, Execute p) => [ Maybe InstructionVal ] -> State p [ Maybe (Register, Int32) ]
execute = mapM execute'

execute' :: (Processor p, Execute p) => Maybe InstructionVal -> State p (Maybe (Register, Int32))
execute' Nothing = do e <- getExecute
                      setExecute $ stall e
                      return Nothing
execute' (Just (Instruction 1 i)) = do e <- getExecute
                                       setExecute $ continue e
                                       execute'' i
execute' (Just i) = undefined -- TODO: Sort out how to do this.

execute'' :: (Processor p, Execute p) => Inst Int32 -> State p (Maybe (Register, Int32))
execute'' (Nop) = return Nothing

execute'' (Add rd ri rj) = return $ Just (rd, ri + rj)

execute'' (Sub rd ri rj) = return $ Just (rd, ri - rj)

execute'' (Mul rd ri rj) = return $ Just (rd, ri * rj)

execute'' (Div rd ri rj) = return $ Just (rd, ri `div` rj)

execute'' (And rd ri rj) = return $ Just (rd, ri .&. rj)

execute'' (Or  rd ri rj) = return $ Just (rd, ri .|. rj)

execute'' (Not rd ri) = return $ Just (rd, complement ri)

execute'' (Jmp ri) = return $ Just (pc, ri)

execute'' (Bez ri c) = if ri == 0
                         then return $ Just (pc, fromIntegral c)
                         else return Nothing

execute'' (Ceq rd ri rj) = return $ Just (rd, if ri == rj
                                                then 1
                                                else 0)

execute'' (Cgt rd ri rj) = return $ Just (rd, if ri > rj
                                                then 1
                                                else 0)

execute'' (Ldc rd c) = return $ Just (rd, fromIntegral c)

execute'' (Ldm rd ri) = do mem <- getDataMemItem (fromIntegral ri) --TODO: this needs to get all four bytes of the memory item.
                           return $ Just (rd, fromIntegral mem)

execute'' (Stm ri rj) = do setDataMemItem (fromIntegral ri) (fromIntegral rj) --TODO: this needs to set all four bytes of the memory item.
                           return Nothing

execute'' (Halt) = undefined --TODO: Halt execution.
