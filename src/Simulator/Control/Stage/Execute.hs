module Simulator.Control.Stage.Execute
  ( execute
  ) where

import Data.Int
import Data.Bits
import Control.Lens
import Control.Monad.State

import Simulator.Data.Processor

execute :: [ Maybe InstructionVal ] -> State Processor [ Maybe (Register, Int32) ]
execute input = do isStalled <- executeStage.stalled
                   if isStalled
                     then use wrbInputLatches >>= return
                     else mapM execute' input

execute' :: Maybe InstructionVal -> State Processor (Maybe (Register, Int32))
execute' Nothing = return Nothing
execute' (Just (Instruction 1 i)) = execute'' i
execute' (Just i) = undefined -- TODO: Sort out how to do this.

execute'' :: Inst Int32 -> State Processor (Maybe (Register, Int32))
execute'' (Nop         ) = return Nothing

execute'' (Add rd ri rj) = return $ Just (rd, ri + rj)

execute'' (Sub rd ri rj) = return $ Just (rd, ri - rj)

execute'' (Mul rd ri rj) = return $ Just (rd, ri * rj)

execute'' (Div rd ri rj) = return $ Just (rd, ri `div` rj)

execute'' (And rd ri rj) = return $ Just (rd, ri .&. rj)

execute'' (Or  rd ri rj) = return $ Just (rd, ri .|. rj)

execute'' (Not rd ri   ) = return $ Just (rd, complement ri)

execute'' (Jmp    ri   ) = return $ Just (pc, ri)

execute'' (Bez    ri  c) = if ri == 0
                             then return $ Just (pc, fromIntegral c)
                             else return Nothing

execute'' (Ceq rd ri rj) = return $ Just (rd, if ri == rj
                                                then 1
                                                else 0)

execute'' (Cgt rd ri rj) = return $ Just (rd, if ri > rj
                                                then 1
                                                else 0)

execute'' (Ldc rd     c) = return $ Just (rd, fromIntegral c)

execute'' (Ldm rd ri   ) = do mem <- getDataMemItem (fromIntegral ri) --TODO: this needs to get all four bytes of the memory item.
                              return $ Just (rd, fromIntegral mem)

execute'' (Stm    ri rj) = do setDataMemItem (fromIntegral ri) (fromIntegral rj) --TODO: this needs to set all four bytes of the memory item.
                              return Nothing

execute'' (Halt        ) = undefined --TODO: Halt execution.
