module Simulator.Control.Stage.Issue
  ( scalarIssue
  , pipelinedIssue
  , superscalarIssue
  ) where

import Data.Int
import Data.Maybe
import Control.Lens
import Control.Monad.State

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
superscalarIssue = do (rs0:rss) <- use $ reservationStations
                      ooo  <- use $ options.outOfOrder
                      rs0' <-       issue' False  rs0
                      rss' <- mapM (issue' True ) rss
                      return undefined
  where issue' outOfOrder rs = undefined
{- TODO: Issue the instructions in-order unless the out-of-order flag is set.
   Check that only one branch is in the eus at any time.
   Check for RaW, WaR and WaW dependencies. -- Should be handled by Tomasulo's algorithm.
   Maximum of 1 instruction issued per cycle per reservation station. -}

checkForDependency :: Inst Register -> Bool -> Maybe [ (Register, Int32) ] -> ProcessorState Bool
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
                     let b = lookup r . fromMaybe [] $ bypass
                     return $ isDirty f && b == Nothing

fillInsts :: Inst Register -> Maybe [ (Register, Int32) ] -> ProcessorState (Inst Int32)
fillInsts i bypass = case i of
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
                         return . searchWithDefault val r . fromMaybe [] $ bypass
