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

scalarExecute :: IssuedData -> ProcessorState ExecutedData
scalarExecute (Nothing                  ) = return Nothing
scalarExecute (Just (Instruction c i, _)) = do simData.insts += 1
                                               simData.cycles += (c - 1)
                                               i' <- execute i
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
pipelinedExecute' (Nothing                  ) = return . Right $ Nothing
pipelinedExecute' (Just (Instruction 1 i, _)) = do
  simData.insts += 1
  fetchStage.stalled.byExecute .= False
  decodeStage.stalled.byExecute .= False
  issueStage.stalled.byExecute .= False
  i' <- execute i
  b  <- use $ options.bypassEnabled
  if b then executeStage.bypassValues .= (Just . maybeToList) i'
       else executeStage.bypassValues .= Nothing
  return . Right . Just $ i'
pipelinedExecute' (Just              (i, rs)) = do
  fetchStage.stalled.byExecute .= True
  decodeStage.stalled.byExecute .= True
  issueStage.stalled.byExecute .= True
  return . Left . Just $ (fmap pred i, rs)

subPipelinedExecute :: IssuedData -> [ (InstructionVal, [ Register ]) ]
                    -> ProcessorState (Either IssuedData ExecutedData)
subPipelinedExecute m_inst ps = do
  let ps' = map (\(i, r) -> (fmap pred i, r)) $ maybeToList m_inst ++ ps
  case findInst ps' of
    Nothing        -> return . Right $ Nothing
    Just (Left  (i, rs)) -> do
      executeStage.subPipeline .= ps'
      b <- use $ options.bypassEnabled
      if b then executeStage.bypassValues .= Just []
           else executeStage.bypassValues .= Nothing
      return . Right $ Nothing
    Just (Right (i, rs)) -> do
      simData.insts += 1
      let ps'' = filter (\(Instruction _ is, _) -> i /= is) ps'
      executeStage.subPipeline .= ps''
      i' <- execute i
      b  <- use $ options.bypassEnabled
      let d = or . map (usesRegister i' . snd) $ ps''
      if b && not d then executeStage.bypassValues .= (Just . maybeToList) i'
                    else executeStage.bypassValues .= Nothing
      return . Right . Just $ i'
  where findInst [] = Nothing
        findInst ps = let (Instruction c i, rs) = minimum ps in
                      if c > 1 then Just . Left  $ (i, rs)
                               else Just . Right $ (i, rs)

superscalarExecute :: [ IssuedData ] -> ProcessorState [ ExecutedData ]
superscalarExecute = undefined

execute :: Inst Int32 -> ProcessorState (Maybe (Register, Int32))
execute i = case i of
  (Nop         ) -> return Nothing
  (Add rd ri rj) -> return . Just $ (rd, ri + rj)
  (Sub rd ri rj) -> return . Just $ (rd, ri - rj)
  (Mul rd ri rj) -> return . Just $ (rd, ri * rj)
  (Div rd ri rj) -> return . Just $ (rd, ri `div` rj)
  (And rd ri rj) -> return . Just $ (rd, ri .&. rj)
  (Or  rd ri rj) -> return . Just $ (rd, ri .|. rj)
  (Not rd ri   ) -> return . Just $ (rd, complement ri)
  (Jmp    ri   ) -> return . Just $ (pc, ri)
  (Bez    ri  c) -> if ri == 0
                     then return . Just $ (pc, fromIntegral c)
                     else return Nothing
  (Ceq rd ri rj) -> return . Just $ (rd, if ri == rj then 1 else 0)
  (Cgt rd ri rj) -> return . Just $ (rd, if ri >  rj then 1 else 0)
  (Ldc rd     c) -> return . Just $ (rd, fromIntegral c)
  (Ldm rd ri   ) -> do mem1 <- use $ dataMem.item (fromIntegral ri    )
                       mem2 <- use $ dataMem.item (fromIntegral ri + 1)
                       mem3 <- use $ dataMem.item (fromIntegral ri + 2)
                       mem4 <- use $ dataMem.item (fromIntegral ri + 3)
                       let b1   = fromIntegral mem1 `shiftL` 24
                           b2   = fromIntegral mem2 `shiftL` 16
                           b3   = fromIntegral mem3 `shiftL` 8
                           b4   = fromIntegral mem4
                           word = b1 .|. b2 .|. b3 .|. b4
                       return $ Just (rd, word)
  (Stm    ri rj) -> do let b1 = fromIntegral (rj `shiftR` 24)
                           b2 = fromIntegral (rj `shiftR` 16)
                           b3 = fromIntegral (rj `shiftR` 8 )
                           b4 = fromIntegral (rj            )
                       dataMem.item (fromIntegral ri    ) .= b1
                       dataMem.item (fromIntegral ri + 1) .= b2
                       dataMem.item (fromIntegral ri + 2) .= b3
                       dataMem.item (fromIntegral ri + 3) .= b4
                       return Nothing
  (Halt        ) -> do halted .= True
                       return Nothing
