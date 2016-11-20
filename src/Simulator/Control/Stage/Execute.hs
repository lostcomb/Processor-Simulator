module Simulator.Control.Stage.Execute
  ( scalarExecute
  , pipelinedExecute
  , superscalarExecute
  ) where

import Data.Int
import Data.Bits
import Data.Maybe
import Control.Lens
import Control.Monad
import Control.Monad.State

import Simulator.Data.Processor
import Simulator.Control.Stall

scalarExecute :: Maybe InstructionVal -> ProcessorState (Maybe (Register, Int32))
scalarExecute input = scalarExecute' input
  where scalarExecute' Nothing = return Nothing
        scalarExecute' (Just (Instruction c i)) = do simData.insts += 1
                                                     simData.cycles += (c - 1)
                                                     execute i

pipelinedExecute :: Maybe InstructionVal -> ProcessorState (Maybe (Register, Int32))
pipelinedExecute input = condM (use $ executeStage.stalled)
  (simData.executeStalledCount += 1 >> use wrbInputLatches) $
  do exeInputLatches .= []
     output <- mapM pipelinedExecute' input
     executeStage.bypassValues .= (map fromJust . filter ((/=) Nothing)) output
     return output
  where pipelinedExecute' Nothing = return Nothing
        pipelinedExecute' (Just (Instruction 1 i)) = do simData.insts += 1
                                                        continueIssue
                                                        execute i
        pipelinedExecute' (Just i) = do let i' = fmap pred i
                                        stallIssue
                                        exeInputLatches %= (++) [ Just i' ]
                                        return Nothing

superscalarExecute :: [ Maybe InstructionVal ] -> ProcessorState [ Maybe (Register, Int32) ]
superscalarExecute = undefined

execute :: Inst Int32 -> ProcessorState (Maybe (Register, Int32))
execute (Nop         ) = return Nothing

execute (Add rd ri rj) = return $ Just (rd, ri + rj)

execute (Sub rd ri rj) = return $ Just (rd, ri - rj)

execute (Mul rd ri rj) = return $ Just (rd, ri * rj)

execute (Div rd ri rj) = return $ Just (rd, ri `div` rj)

execute (And rd ri rj) = return $ Just (rd, ri .&. rj)

execute (Or  rd ri rj) = return $ Just (rd, ri .|. rj)

execute (Not rd ri   ) = return $ Just (rd, complement ri)

execute (Jmp    ri   ) = return $ Just (pc, ri)

execute (Bez    ri  c) = if ri == 0
                           then return $ Just (pc, fromIntegral c)
                           else return Nothing

execute (Ceq rd ri rj) = return $ Just (rd, if ri == rj
                                              then 1
                                              else 0)

execute (Cgt rd ri rj) = return $ Just (rd, if ri > rj
                                              then 1
                                              else 0)

execute (Ldc rd     c) = return $ Just (rd, fromIntegral c)

execute (Ldm rd ri   ) = do mem1 <- use $ dataMem.item (fromIntegral ri    )
                            mem2 <- use $ dataMem.item (fromIntegral ri + 1)
                            mem3 <- use $ dataMem.item (fromIntegral ri + 2)
                            mem4 <- use $ dataMem.item (fromIntegral ri + 3)
                            let b1   = fromIntegral mem1 `shiftL` 24
                                b2   = fromIntegral mem2 `shiftL` 16
                                b3   = fromIntegral mem3 `shiftL` 8
                                b4   = fromIntegral mem4
                                word = b1 .|. b2 .|. b3 .|. b4
                            return $ Just (rd, word)

execute (Stm    ri rj) = do let b1 = fromIntegral (rj `shiftR` 24)
                                b2 = fromIntegral (rj `shiftR` 16)
                                b3 = fromIntegral (rj `shiftR` 8 )
                                b4 = fromIntegral (rj            )
                            dataMem.item (fromIntegral ri    ) .= b1
                            dataMem.item (fromIntegral ri + 1) .= b2
                            dataMem.item (fromIntegral ri + 2) .= b3
                            dataMem.item (fromIntegral ri + 3) .= b4
                            return Nothing

execute (Halt        ) = do halted .= True
                            return Nothing
