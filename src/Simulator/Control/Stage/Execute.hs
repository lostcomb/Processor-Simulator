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
import Simulator.Control.BranchPrediction

scalarExecute :: IssuedData -> ProcessorState ExecutedData
scalarExecute (Nothing                            ) = return Nothing
scalarExecute (Just (index, Instruction c i co, _)) = do simData.insts += 1
                                                         simData.cycles += (c - 1)
                                                         i' <- execute index i co
                                                         return . Just $ i'

pipelinedExecute :: IssuedData -> ProcessorState (Either IssuedData ExecutedData)
pipelinedExecute i = do
  p <- use $ options.pipelinedEUs
  if p then do
    ps <- use $ executeStage.subPipeline
    subPipelinedExecute i ps
  else do
    pipelinedExecute' i

pipelinedExecute' :: IssuedData -> ProcessorState (Either IssuedData ExecutedData)
pipelinedExecute' (Nothing                            ) = return . Right $ Nothing
pipelinedExecute' (Just (index, Instruction 1 i co, _)) = do
  simData.insts += 1
  fetchStage.stalled.byExecute .= False
  decodeStage.stalled.byExecute .= False
  issueStage.stalled.byExecute .= False
  (index', i', inv) <- execute index i co
  b         <- use $ options.bypassEnabled
  if b then executeStage.bypassValues .= (Just . maybeToList) i'
       else executeStage.bypassValues .= Nothing
  return . Right . Just $ (index', i', inv)
pipelinedExecute' (Just                 (index, i, rs)) = do
  fetchStage.stalled.byExecute .= True
  decodeStage.stalled.byExecute .= True
  issueStage.stalled.byExecute .= True
  return . Left . Just $ (index, fmap pred i, rs)

subPipelinedExecute :: IssuedData -> [ (Int, InstructionVal, [ Register ]) ]
                    -> ProcessorState (Either IssuedData ExecutedData)
subPipelinedExecute m_inst ps = do
  let ps' = map (\(index, i, r) -> (index, fmap pred i, r)) $ maybeToList m_inst ++ ps
  case findInst ps' of
    Nothing                  -> return . Right $ Nothing
    Just (Left  (index, i, co, rs)) -> do
      executeStage.subPipeline .= ps'
      b <- use $ options.bypassEnabled
      if b then executeStage.bypassValues .= Just []
           else executeStage.bypassValues .= Nothing
      return . Right $ Nothing
    Just (Right (index, i, co, rs)) -> do
      simData.insts += 1
      let ps'' = filter (\(_, Instruction _ is _, _) -> i /= is) ps'
      executeStage.subPipeline .= ps''
      (index', i', inv) <- execute index i co
      b                 <- use $ options.bypassEnabled
      let d = or . map (usesRegister i' . (\(_, _, a) -> a)) $ ps''
      if b && not d then executeStage.bypassValues .= (Just . maybeToList) i'
                    else executeStage.bypassValues .= Nothing
      return . Right . Just $ (index', i', inv)
  where findInst [] = Nothing
        findInst ps = let (index, Instruction c i co, rs) = minimum ps in
                      if c > 1 then Just . Left  $ (index, i, co, rs)
                               else Just . Right $ (index, i, co, rs)

superscalarExecute :: [ IssuedData ] -> ProcessorState [ Either IssuedData ExecutedData ]
superscalarExecute = mapM pipelinedExecute --TODO: Change non-sub-pipelined version to just make the reservation station busy.

execute :: Int -> Inst Int32 -> Control -> ProcessorState (Int, Maybe (Register, Int32), Bool)
execute index i co = case i of
  (Nop         ) -> return (index, Nothing                 , False)
  (Add rd ri rj) -> return (index, Just (rd, ri + rj      ), False)
  (Sub rd ri rj) -> return (index, Just (rd, ri - rj      ), False)
  (Mul rd ri rj) -> return (index, Just (rd, ri * rj      ), False)
  (Div rd ri rj) -> return (index, Just (rd, ri `div` rj  ), False)
  (And rd ri rj) -> return (index, Just (rd, ri .&. rj    ), False)
  (Or  rd ri rj) -> return (index, Just (rd, ri .|. rj    ), False)
  (Not rd ri   ) -> return (index, Just (rd, complement ri), False)
  (Jmp    ri   ) -> do branch True (Just . fromIntegral $ ri) co
                       let taken = if isTaken co then getTarget co == fromIntegral ri
                                                 else False
                       simData.predictions %= (++) [ (Jmp ri, taken) ]
                       if taken then simData.hitPredictions += 1
                                else simData.misPredictions += 1
                       return (index, Just (pc, ri), not taken)
  (Bez    ri  c) -> do let target = if ri == 0 then Just (fromIntegral c)
                                               else Nothing
                           result = if ri == 0 then Just (pc, fromIntegral c)
                                               else Nothing
                           inv    = (ri == 0) `xor` isTaken co
                       branch (ri == 0) target co
                       simData.predictions %= (++) [ (Bez ri c, not inv) ]
                       if not inv then simData.hitPredictions += 1
                                  else simData.misPredictions += 1
                       return (index, result, inv)
  (Ceq rd ri rj) -> return (index, Just (rd, if ri == rj then 1 else 0), False)
  (Cgt rd ri rj) -> return (index, Just (rd, if ri >  rj then 1 else 0), False)
  (Ldc rd     c) -> return (index, Just (rd, fromIntegral c           ), False)
  (Ldm rd ri   ) -> do mem1 <- use $ dataMem.item (fromIntegral ri    )
                       mem2 <- use $ dataMem.item (fromIntegral ri + 1)
                       mem3 <- use $ dataMem.item (fromIntegral ri + 2)
                       mem4 <- use $ dataMem.item (fromIntegral ri + 3)
                       let b1   = fromIntegral mem1 `shiftL` 24
                           b2   = fromIntegral mem2 `shiftL` 16
                           b3   = fromIntegral mem3 `shiftL` 8
                           b4   = fromIntegral mem4
                           word = b1 .|. b2 .|. b3 .|. b4
                       return (index, Just (rd, word), False)
  (Stm    ri rj) -> do let b1 = fromIntegral (rj `shiftR` 24)
                           b2 = fromIntegral (rj `shiftR` 16)
                           b3 = fromIntegral (rj `shiftR` 8 )
                           b4 = fromIntegral (rj            )
                       dataMem.item (fromIntegral ri    ) .= b1
                       dataMem.item (fromIntegral ri + 1) .= b2
                       dataMem.item (fromIntegral ri + 2) .= b3
                       dataMem.item (fromIntegral ri + 3) .= b4
                       return (index, Nothing, False)
  (Halt        ) -> do halted .= True
                       return (index, Nothing, False)
