{-# LANGUAGE FlexibleContexts #-}
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

import Simulator.Data.Setter
import Simulator.Data.Processor
import Simulator.Control.BranchPrediction

scalarExecute :: IssuedData -> ProcessorState ExecutedData
scalarExecute (Nothing                            ) = return Nothing
scalarExecute (Just (inst_id, Instruction c i co, _)) = do simData.cycles += (c - 1)
                                                           i' <- execute inst_id i co
                                                           return . Just $ i'

pipelinedExecute :: IssuedData -> ProcessorState (Either IssuedData ExecutedData)
pipelinedExecute i = do
  p <- use $ options.pipelinedEUs
  b <- use $ options.bypassEnabled
  if b then executeStage.bypassValues .= Just []
       else executeStage.bypassValues .= Nothing
  if p then do
    ps <- use $ executeStage.subPipeline
    subPipelinedExecute i (0, head ps)
  else do
    pipelinedExecute' stall continue i
  where stall    = do fetchStage.stalled.byExecute  .= True
                      decodeStage.stalled.byExecute .= True
                      issueStage.stalled.byExecute  .= True
        continue = do fetchStage.stalled.byExecute  .= False
                      decodeStage.stalled.byExecute .= False
                      issueStage.stalled.byExecute  .= False

pipelinedExecute' :: ProcessorState () -> ProcessorState () -> IssuedData
                  -> ProcessorState (Either IssuedData ExecutedData)
pipelinedExecute' _     _        (Nothing                              ) = return . Right $ Nothing
pipelinedExecute' _     continue (Just (inst_id, Instruction 1 i co, _)) = do
  continue
  (inst_id', i', inv) <- execute inst_id i co
  b <- use $ options.bypassEnabled
  when b $
    executeStage.bypassValues ++?= (Just . instToBypass inst_id) i'
  return . Right . Just $ (inst_id', i', inv)
pipelinedExecute' stall _        (Just                 (inst_id, i, rs)) = do
  stall
  return . Left . Just $ (inst_id, fmap pred i, rs)

subPipelinedExecute :: IssuedData -> (Int, [ (Int, InstructionVal, [ Register ]) ])
                    -> ProcessorState (Either IssuedData ExecutedData)
subPipelinedExecute m_inst (index, ps) = do
  let ps' = map (\(inst_id, i, r) -> (inst_id, fmap pred i, r)) $ maybeToList m_inst ++ ps
  case findInst ps' of
    Nothing                  -> return . Right $ Nothing
    Just (Left  (inst_id, i, co, rs)) -> do
      executeStage.subPipeline %= (update index ps')
      return . Right $ Nothing
    Just (Right (inst_id, i, co, rs)) -> do
      let ps'' = filter (\(_, Instruction _ is _, _) -> i /= is) ps'
      executeStage.subPipeline %= (update index ps'')
      (inst_id', i', inv) <- execute inst_id i co
      b <- use $ options.bypassEnabled
      let d = or . map (usesRegister i' . (\(_, _, a) -> a)) $ ps''
      when (b && not d) $
        executeStage.bypassValues ++?= (Just . instToBypass inst_id) i'
      return . Right . Just $ (inst_id', i', inv)
  where findInst [] = Nothing
        findInst ps = let (inst_id, Instruction c i co, rs) = minimum ps in
                      if c > 1 then Just . Left  $ (inst_id, i, co, rs)
                               else Just . Right $ (inst_id, i, co, rs)

-- |This function returns a list containing the bypass values.
instToBypass :: Int -> Maybe (Register, Int32) -> [ (Int, Register, Int32) ]
instToBypass _   (Nothing    ) = []
instToBypass iid (Just (r, v)) = [ (iid, r, v) ]

superscalarExecute :: [ IssuedData ] -> ProcessorState [ Either IssuedData ExecutedData ]
superscalarExecute i = do
  p <- use $ options.pipelinedEUs
  b <- use $ options.bypassEnabled
  if b then executeStage.bypassValues .= Just []
       else executeStage.bypassValues .= Nothing
  if p then do
    ps <- use $ executeStage.subPipeline
    mapM (\(iData, index, sp) -> subPipelinedExecute iData (index, sp)) $ zip3 i [0,1..] ps
  else do
    mapM (\(index, iData) -> pipelinedExecute' (stall index) (continue index) iData) $ zip [0,1..] i
  where stall    index = do rss <- use $ reservationStations
                            let (rs, _, i) = rss !! index
                            reservationStations %= (update index (rs, True, i))
        continue index = do rss <- use $ reservationStations
                            let (rs, _, i) = rss !! index
                            reservationStations %= (update index (rs, False, i))

execute :: Int -> Inst Int32 -> Control -> ProcessorState (Int, Maybe (Register, Int32), Bool)
execute inst_id i co = case i of
  (Nop         ) -> return (inst_id, Nothing                 , False)
  (Add rd ri rj) -> return (inst_id, Just (rd, ri + rj      ), False)
  (Sub rd ri rj) -> return (inst_id, Just (rd, ri - rj      ), False)
  (Mul rd ri rj) -> return (inst_id, Just (rd, ri * rj      ), False)
  (Div rd ri rj) -> return (inst_id, Just (rd, ri `div` rj  ), False)
  (And rd ri rj) -> return (inst_id, Just (rd, ri .&. rj    ), False)
  (Or  rd ri rj) -> return (inst_id, Just (rd, ri .|. rj    ), False)
  (Not rd ri   ) -> return (inst_id, Just (rd, complement ri), False)
  (Jmp    ri   ) -> do branch True (Just . fromIntegral $ ri) co
                       let taken = if isTaken co then getTarget co == fromIntegral ri
                                                 else False
                       simData.predictions ++= [ (Jmp ri, taken) ]
                       if taken then simData.hitPredictions += 1
                                else simData.misPredictions += 1
                       return (inst_id, Just (pc, ri), not taken)
  (Bez    ri  c) -> do let target = if ri == 0 then Just (fromIntegral c)
                                               else Nothing
                           result = if ri == 0 then Just (pc, fromIntegral c)
                                               else Just (pc, fromIntegral (getPC co + instLength))
                           inv    = (ri == 0) `xor` isTaken co
                       branch (ri == 0) target co
                       simData.predictions ++= [ (Bez ri c, not inv) ]
                       if not inv then simData.hitPredictions += 1
                                  else simData.misPredictions += 1
                       return (inst_id, result, inv)
  (Ceq rd ri rj) -> return (inst_id, Just (rd, if ri == rj then 1 else 0), False)
  (Cgt rd ri rj) -> return (inst_id, Just (rd, if ri >  rj then 1 else 0), False)
  (Ldc rd     c) -> return (inst_id, Just (rd, fromIntegral c           ), False)
  (Ldm rd ri   ) -> do mem1 <- use $ dataMem.item (fromIntegral ri    )
                       mem2 <- use $ dataMem.item (fromIntegral ri + 1)
                       mem3 <- use $ dataMem.item (fromIntegral ri + 2)
                       mem4 <- use $ dataMem.item (fromIntegral ri + 3)
                       let b1   = fromIntegral mem1 `shiftL` 24
                           b2   = fromIntegral mem2 `shiftL` 16
                           b3   = fromIntegral mem3 `shiftL` 8
                           b4   = fromIntegral mem4
                           word = b1 .|. b2 .|. b3 .|. b4
                       return (inst_id, Just (rd, word), False)
  (Stm    ri rj) -> do let b1 = fromIntegral (rj `shiftR` 24)
                           b2 = fromIntegral (rj `shiftR` 16)
                           b3 = fromIntegral (rj `shiftR` 8 )
                           b4 = fromIntegral (rj            )
                       dataMem.item (fromIntegral ri    ) .= b1
                       dataMem.item (fromIntegral ri + 1) .= b2
                       dataMem.item (fromIntegral ri + 2) .= b3
                       dataMem.item (fromIntegral ri + 3) .= b4
                       return (inst_id, Nothing, False)
  (Halt        ) -> do pt <- use $ options.procType
                       if pt /= Superscalar then halted .= True
                                            else writebackStage.haltAfter .= Just inst_id
                       return (inst_id, Nothing, False)

-- |This function updates the value at index @n@ with @val@.
update :: Int -> a -> [ a ] -> [ a ]
update n val xs = take n xs ++ val : drop (n + 1) xs
