module Simulator.Control.Stage.Issue
  ( scalarIssue
  , pipelinedIssue
  , superscalarIssue
  ) where

import Data.Int
import Data.List (delete)
import Data.Maybe
import Control.Lens
import Control.Monad.State
import Control.Applicative (pure, (<$), (<$>), (<*), (<*>), (*>))

import Simulator.Data.Processor
import Simulator.Data.Association

scalarIssue :: DecodedData -> ProcessorState IssuedData
scalarIssue (Nothing                  )
  = return Nothing
scalarIssue (Just (Instruction c i co))
  = do i' <- fillInsts i Nothing
       return . Just $ (0, Instruction c i' co, instOperands i)

pipelinedIssue :: DecodedData -> ProcessorState IssuedData
pipelinedIssue (Nothing                  ) = return Nothing
pipelinedIssue (Just (Instruction c i co)) = do
          bs <- use $ executeStage.bypassValues
          s  <- use $ issueStage.speculative
          d  <- checkForDependency i s bs
          issueStage.speculative .= (isJmp i || isBez i)
          if d then do
            simData.issueStalledCount += 1
            fetchStage.stalled.byIssue .= True
            decodeStage.stalled.byIssue .= True
            return Nothing
          else do
            fetchStage.stalled.byIssue .= False
            decodeStage.stalled.byIssue .= False
            i' <- fillInsts i bs
            return . Just $ (0, Instruction c i' co, instOperands i)

superscalarIssue :: ProcessorState [ IssuedData ]
superscalarIssue = do (rs0:rss)  <- use $ reservationStations
                      ooo        <- use $ options.outOfOrder
                      (rs0',  m) <-       issue' False  rs0
                      i'         <- mapM (issue' ooo  ) rss
                      let (rss', ms) = unzip i'
                      reservationStations .= (rs0':rss')
                      return (m:ms)
  where issue' outOfOrder (rs, busy, n) = do
          u  <- use $ options.unAlignedIssue
          s  <- use $ options.issueWindowSize
          ss <- use $ options.shelfSize
          bs <- use $ executeStage.bypassValues
          let size         = min s ss
              window       = if u then take (n + 1) rs else take s rs
              (ids, _, vs) = unzip3 . fromMaybe [] $ bs
              bypass       = zip ids vs
              insts        = if outOfOrder then filter    (ready bypass) window
                                           else takeWhile (ready bypass) window
          if not busy && length insts > 0 then do
            let rs' = delete (head insts) rs
            i' <- rsEntryToInst (head insts) bypass
            return ((rs', busy, (n - 1) `mod` size), Just i')
          else do
            simData.issueStalledCount += 1
            return ((rs , busy, n), Nothing)

-- |This function returns True if the specified reservation station entry is
--  ready to be issued.
ready :: [ (Int, Int32) ] -> ReservationStationEntry -> Bool
ready bypass (_, _, qi, qj, _, _) = got qi && got qj
  where got x = case x of
          Just inst_id -> lookup inst_id bypass /= Nothing
          Nothing      -> True

checkForDependency :: Inst Register -> Bool -> Maybe [ (Int, Register, Int32) ]
                   -> ProcessorState Bool
checkForDependency i speculative bypass = case i of
  (Nop         ) -> return False
  (Add rd ri rj) -> (||) <$> check ri <*> check rj
  (Sub rd ri rj) -> (||) <$> check ri <*> check rj
  (Mul rd ri rj) -> (||) <$> check ri <*> check rj
  (Div rd ri rj) -> (||) <$> check ri <*> check rj
  (And rd ri rj) -> (||) <$> check ri <*> check rj
  (Or  rd ri rj) -> (||) <$> check ri <*> check rj
  (Not rd ri   ) -> check ri
  (Jmp    ri   ) -> check ri
  (Bez    ri  o) -> check ri
  (Ceq rd ri rj) -> (||) <$> check ri <*> check rj
  (Cgt rd ri rj) -> (||) <$> check ri <*> check rj
  (Ldc rd     c) -> return False
  (Ldm rd ri   ) -> check ri
  (Stm    ri rj) -> ((||) . (||) speculative) <$> check ri <*> check rj
  (Halt        ) -> return False
  where check :: Register -> ProcessorState Bool
        check r = do f <- use (regFile.regFlag r)
                     let (_, rs, vs) = unzip3 . fromMaybe [] $ bypass
                         b = lookup r . zip rs $ vs
                     return $ isDirty f && b == Nothing

fillInsts :: Inst Register -> Maybe [ (Int, Register, Int32) ] -> ProcessorState (Inst Int32)
fillInsts i bypass = do
  simData.issInsts += 1
  case i of
    (Nop         ) -> return Nop
    (Add rd ri rj) -> Add <$> stainReg rd <*> updateReg ri <*> updateReg rj
    (Sub rd ri rj) -> Sub <$> stainReg rd <*> updateReg ri <*> updateReg rj
    (Mul rd ri rj) -> Mul <$> stainReg rd <*> updateReg ri <*> updateReg rj
    (Div rd ri rj) -> Div <$> stainReg rd <*> updateReg ri <*> updateReg rj
    (And rd ri rj) -> And <$> stainReg rd <*> updateReg ri <*> updateReg rj
    (Or  rd ri rj) -> Or  <$> stainReg rd <*> updateReg ri <*> updateReg rj
    (Not rd ri   ) -> Not <$> stainReg rd <*> updateReg ri
    (Jmp    ri   ) -> Jmp <$  stainReg pc <*> updateReg ri
    (Bez    ri  o) -> Bez <$  stainReg pc <*> updateReg ri <*> return o
    (Ceq rd ri rj) -> Ceq <$> stainReg rd <*> updateReg ri <*> updateReg rj
    (Cgt rd ri rj) -> Cgt <$> stainReg rd <*> updateReg ri <*> updateReg rj
    (Ldc rd     o) -> Ldc <$> stainReg rd                  <*> return o
    (Ldm rd ri   ) -> Ldm <$> stainReg rd <*> updateReg ri
    (Stm    ri rj) -> Stm <$>                 updateReg ri <*> updateReg rj
    (Halt        ) -> return Halt
  where stainReg :: Register -> ProcessorState Register
        stainReg  r = do regFile.regFlag r += 1
                         return r
        updateReg :: Register -> ProcessorState Int32
        updateReg r = do val <- use $ regFile.regVal r
                         let (_, rs, vs) = unzip3 . fromMaybe [] $ bypass
                         return . searchWithDefault val r . zip rs $ vs

rsEntryToInst :: ReservationStationEntry -> [ (Int, Int32) ]
              -> ProcessorState (Int, InstructionVal, [ Register ])
rsEntryToInst (inst_id, Instruction c i co, qi, qj, vi, vj) bypass
  = do simData.issInsts += 1
       return (inst_id, Instruction c i' co, instOperands i)
  where vi' = case qi of
                (Just key  ) -> fromJust . lookup key $ bypass
                (Nothing   ) -> vi
        vj' = case qj of
                (Just key  ) -> fromJust . lookup key $ bypass
                (Nothing   ) -> vj
        i'  = case i of
                (Nop       ) -> Nop
                (Add rd _ _) -> Add rd vi' vj'
                (Sub rd _ _) -> Sub rd vi' vj'
                (Mul rd _ _) -> Mul rd vi' vj'
                (Div rd _ _) -> Div rd vi' vj'
                (And rd _ _) -> And rd vi' vj'
                (Or  rd _ _) -> Or  rd vi' vj'
                (Not rd _  ) -> Not rd vi'
                (Jmp    _  ) -> Jmp    vi'
                (Bez    _ o) -> Bez    vi' o
                (Ceq rd _ _) -> Ceq rd vi' vj'
                (Cgt rd _ _) -> Cgt rd vi' vj'
                (Ldc rd   o) -> Ldc rd     o
                (Ldm rd _  ) -> Ldm rd vi'
                (Stm    _ _) -> Stm    vi' vj'
                (Halt      ) -> Halt
