module Simulator.Control.Stage.Issue
  ( scalarIssue
  , pipelinedIssue
  , superscalarIssue
  ) where

import Data.Int
import Control.Lens
import Control.Monad.State

import Simulator.Data.Processor
import Simulator.Control.Stall

scalarIssue :: Maybe InstructionReg -> ProcessorState (Maybe InstructionVal)
scalarIssue input = issue input
  where issue (Nothing               ) = return Nothing
        issue (Just (Instruction c i)) = do i' <- fillInsts i []
                                            return . Just . Instruction c $ i'

pipelinedIssue :: Maybe InstructionReg -> ProcessorState (Maybe InstructionVal)
pipelinedIssue input = do
  s <- use $ issueStage.stalled
  if s then do
    simData.issueStalledCount += 1
    latches <- use exeInputLatches
    return . head $ latches
  else do
    issue input
  where issue (Nothing               ) = return Nothing
        issue (Just (Instruction c i)) = do
          b  <- use $ options.bypassEnabled
          bs <- use $ executeStage.bypassValues
          d  <- if b then checkForDependency i bs
                     else checkForDependency i [] -- Move this to the execute.
          if d then do
            stallIssue
            return Nothing
          else do
            i' <- if b then fillInsts i bs
                       else fillInsts i []
            return . Just . Instruction c $ i'

superscalarIssue :: [ Maybe InstructionReg ] -> ProcessorState [ Maybe InstructionVal ]
superscalarIssue = undefined

checkForDependency :: Inst Register -> [ (Register, Int32) ] -> ProcessorState Bool
checkForDependency i bypass = case i of
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
  (Stm    ri rj) -> (||) <$> check ri <*> check rj
  (Halt        ) -> return False
  where check :: Register -> ProcessorState Bool
        check r = do f <- use (regFile.regFlag r)
                     let b = lookup r bypass
                     return $ f == Dirty && b == Nothing

fillInsts :: Inst Register -> [ (Register, Int32) ] -> ProcessorState (Inst Int32)
fillInsts i bypass = case i of
  (Nop         ) -> return Nop
  (Add rd ri rj) -> Add <$> stainReg rd <*> updateReg ri <*> updateReg rj
  (Sub rd ri rj) -> Sub <$> stainReg rd <*> updateReg ri <*> updateReg rj
  (Mul rd ri rj) -> Mul <$> stainReg rd <*> updateReg ri <*> updateReg rj
  (Div rd ri rj) -> Div <$> stainReg rd <*> updateReg ri <*> updateReg rj
  (And rd ri rj) -> And <$> stainReg rd <*> updateReg ri <*> updateReg rj
  (Or  rd ri rj) -> Or  <$> stainReg rd <*> updateReg ri <*> updateReg rj
  (Not rd ri   ) -> Not <$> stainReg rd <*> updateReg ri
  (Jmp    ri   ) -> Jmp <$>                 updateReg ri
  (Bez    ri  o) -> Bez <$>                 updateReg ri <*> return o
  (Ceq rd ri rj) -> Ceq <$> stainReg rd <*> updateReg ri <*> updateReg rj
  (Cgt rd ri rj) -> Cgt <$> stainReg rd <*> updateReg ri <*> updateReg rj
  (Ldc rd     o) -> Ldc <$> stainReg rd                  <*> return o
  (Ldm rd ri   ) -> Ldm <$> stainReg rd <*> updateReg ri
  (Stm    ri rj) -> Stm <$>                 updateReg ri <*> updateReg rj
  (Halt        ) -> return Halt
  where stainReg :: Register -> ProcessorState Register
        stainReg  r = do regFile.regFlag r .= Dirty
                         return r
        updateReg :: Register -> ProcessorState Int32
        updateReg r = do val <- use $ regFile.regVal r
                         return . findWithDefault val r $ bypass

findWithDefault :: (Eq a) => b -> a -> [ (a, b) ] -> b
findWithDefault def x []            = def
findWithDefault def x ((x', y) : l)
  | x == x'                         = y
  | otherwise                       = findWithDefault def x l
