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
scalarExecute (Nothing                     ) = return Nothing
scalarExecute (Just (Instruction c i co, _)) = do simData.insts += 1
                                                  simData.cycles += (c - 1)
                                                  i' <- execute i co
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
pipelinedExecute' (Nothing                     ) = return . Right $ Nothing
pipelinedExecute' (Just (Instruction 1 i co, _)) = do
  simData.insts += 1
  fetchStage.stalled.byExecute .= False
  decodeStage.stalled.byExecute .= False
  issueStage.stalled.byExecute .= False
  (i', inv) <- execute i co
  b         <- use $ options.bypassEnabled
  if b then executeStage.bypassValues .= (Just . maybeToList) i'
       else executeStage.bypassValues .= Nothing
  return . Right . Just $ (i', inv)
pipelinedExecute' (Just                 (i, rs)) = do
  fetchStage.stalled.byExecute .= True
  decodeStage.stalled.byExecute .= True
  issueStage.stalled.byExecute .= True
  return . Left . Just $ (fmap pred i, rs)

subPipelinedExecute :: IssuedData -> [ (InstructionVal, [ Register ]) ]
                    -> ProcessorState (Either IssuedData ExecutedData)
subPipelinedExecute m_inst ps = do
  let ps' = map (\(i, r) -> (fmap pred i, r)) $ maybeToList m_inst ++ ps
  case findInst ps' of
    Nothing                  -> return . Right $ Nothing
    Just (Left  (i, co, rs)) -> do
      executeStage.subPipeline .= ps'
      b <- use $ options.bypassEnabled
      if b then executeStage.bypassValues .= Just []
           else executeStage.bypassValues .= Nothing
      return . Right $ Nothing
    Just (Right (i, co, rs)) -> do
      simData.insts += 1
      let ps'' = filter (\(Instruction _ is _, _) -> i /= is) ps'
      executeStage.subPipeline .= ps''
      (i', inv) <- execute i co
      b         <- use $ options.bypassEnabled
      let d = or . map (usesRegister i' . snd) $ ps''
      if b && not d then executeStage.bypassValues .= (Just . maybeToList) i'
                    else executeStage.bypassValues .= Nothing
      return . Right . Just $ (i', inv)
  where findInst [] = Nothing
        findInst ps = let (Instruction c i co, rs) = minimum ps in
                      if c > 1 then Just . Left  $ (i, co, rs)
                               else Just . Right $ (i, co, rs)

superscalarExecute :: [ IssuedData ] -> ProcessorState [ ExecutedData ]
superscalarExecute = undefined

execute :: Inst Int32 -> Control -> ProcessorState (Maybe (Register, Int32), Bool)
execute i co = case i of
  (Nop         ) -> return (Nothing                 , False)
  (Add rd ri rj) -> return (Just (rd, ri + rj      ), False)
  (Sub rd ri rj) -> return (Just (rd, ri - rj      ), False)
  (Mul rd ri rj) -> return (Just (rd, ri * rj      ), False)
  (Div rd ri rj) -> return (Just (rd, ri `div` rj  ), False)
  (And rd ri rj) -> return (Just (rd, ri .&. rj    ), False)
  (Or  rd ri rj) -> return (Just (rd, ri .|. rj    ), False)
  (Not rd ri   ) -> return (Just (rd, complement ri), False)
  (Jmp    ri   ) -> do branch True (Just . fromIntegral $ ri) co
                       simData.predictions %= (++) [ (Jmp ri, isTaken co) ]
                       if isTaken co then simData.hitPredictions += 1
                                     else simData.misPredictions += 1
                       return (Just (pc, ri), not . isTaken $ co)
  (Bez    ri  c) -> do let target = if ri == 0 then Just (fromIntegral c)
                                               else Nothing
                           result = if ri == 0 then Just (pc, fromIntegral c)
                                               else Nothing
                           inv    = (ri == 0) `xor` isTaken co
                       branch (ri == 0) target co
                       simData.predictions %= (++) [ (Bez ri c, not inv) ]
                       if not inv then simData.hitPredictions += 1
                                  else simData.misPredictions += 1
                       return (result, inv)
  (Ceq rd ri rj) -> return (Just (rd, if ri == rj then 1 else 0), False)
  (Cgt rd ri rj) -> return (Just (rd, if ri >  rj then 1 else 0), False)
  (Ldc rd     c) -> return (Just (rd, fromIntegral c           ), False)
  (Ldm rd ri   ) -> do mem1 <- use $ dataMem.item (fromIntegral ri    )
                       mem2 <- use $ dataMem.item (fromIntegral ri + 1)
                       mem3 <- use $ dataMem.item (fromIntegral ri + 2)
                       mem4 <- use $ dataMem.item (fromIntegral ri + 3)
                       let b1   = fromIntegral mem1 `shiftL` 24
                           b2   = fromIntegral mem2 `shiftL` 16
                           b3   = fromIntegral mem3 `shiftL` 8
                           b4   = fromIntegral mem4
                           word = b1 .|. b2 .|. b3 .|. b4
                       return (Just (rd, word), False)
  (Stm    ri rj) -> do let b1 = fromIntegral (rj `shiftR` 24)
                           b2 = fromIntegral (rj `shiftR` 16)
                           b3 = fromIntegral (rj `shiftR` 8 )
                           b4 = fromIntegral (rj            )
                       dataMem.item (fromIntegral ri    ) .= b1
                       dataMem.item (fromIntegral ri + 1) .= b2
                       dataMem.item (fromIntegral ri + 2) .= b3
                       dataMem.item (fromIntegral ri + 3) .= b4
                       return (Nothing, False)
  (Halt        ) -> do halted .= True
                       return (Nothing, False)
