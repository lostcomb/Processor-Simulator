module Simulator.Control.Stage.Issue
  ( scalarIssue
  , pipelinedIssue
  ) where

import Data.Int
import Control.Lens
import Control.Monad.State

import Simulator.Data.Processor
import Simulator.Control.Stall

scalarIssue :: [ Maybe InstructionReg ] -> ProcessorState [ Maybe InstructionVal ]
scalarIssue input = mapM issue input

pipelinedIssue :: [ Maybe InstructionReg ] -> ProcessorState [ Maybe InstructionVal ]
pipelinedIssue input = condM (use $ issueStage.stalled)
  (simData.issueStalledCount += 1 >> use exeInputLatches) $
  do mapM issue input

issue :: Maybe InstructionReg -> ProcessorState (Maybe InstructionVal)
issue Nothing = return Nothing
issue (Just (Instruction c i)) = condM (checkForDependency i)
  (stallIssue >> return Nothing) $
  do inst <- fillInsts i
     return $ Just $ Instruction c inst

checkForDependency :: Inst Register -> ProcessorState Bool
checkForDependency i = case i of
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
        check r = use (regFile.regFlag r) >>= (\f -> return $ f == Dirty)

fillInsts :: Inst Register -> ProcessorState (Inst Int32)
fillInsts (Nop         ) =    return $ Nop
fillInsts (Add rd ri rj) = do regFile.regFlag rd .= Dirty
                              vi <- getRegVal ri
                              vj <- getRegVal rj
                              return $ Add rd vi vj
fillInsts (Sub rd ri rj) = do regFile.regFlag rd .= Dirty
                              vi <- getRegVal ri
                              vj <- getRegVal rj
                              return $ Sub rd vi vj
fillInsts (Mul rd ri rj) = do regFile.regFlag rd .= Dirty
                              vi <- getRegVal ri
                              vj <- getRegVal rj
                              return $ Mul rd vi vj
fillInsts (Div rd ri rj) = do regFile.regFlag rd .= Dirty
                              vi <- getRegVal ri
                              vj <- getRegVal rj
                              return $ Div rd vi vj
fillInsts (And rd ri rj) = do regFile.regFlag rd .= Dirty
                              vi <- getRegVal ri
                              vj <- getRegVal rj
                              return $ And rd vi vj
fillInsts (Or  rd ri rj) = do regFile.regFlag rd .= Dirty
                              vi <- getRegVal ri
                              vj <- getRegVal rj
                              return $ Or rd vi vj
fillInsts (Not rd ri   ) = do regFile.regFlag rd .= Dirty
                              vi <- getRegVal ri
                              return $ Not rd vi
fillInsts (Jmp    ri   ) = do vi <- getRegVal ri
                              return $ Jmp vi
fillInsts (Bez    ri  o) = do vi <- getRegVal ri
                              return $ Bez vi o
fillInsts (Ceq rd ri rj) = do regFile.regFlag rd .= Dirty
                              vi <- getRegVal ri
                              vj <- getRegVal rj
                              return $ Ceq rd vi vj
fillInsts (Cgt rd ri rj) = do regFile.regFlag rd .= Dirty
                              vi <- getRegVal ri
                              vj <- getRegVal rj
                              return $ Cgt rd vi vj
fillInsts (Ldc rd     c) = do regFile.regFlag rd .= Dirty
                              return $ Ldc rd c
fillInsts (Ldm rd ri   ) = do regFile.regFlag rd .= Dirty
                              vi <- getRegVal ri
                              return $ Ldm rd vi
fillInsts (Stm    ri rj) = do vi <- getRegVal ri
                              vj <- getRegVal rj
                              return $ Stm vi vj
fillInsts (Halt        ) =    return $ Halt

getRegVal :: Register -> ProcessorState Int32
getRegVal r = do isBypassEnabled <- use $ options.bypassEnabled
                 val <- use $ regFile.regVal r
                 if isBypassEnabled
                   then do bypass <- use $ executeStage.bypassValues
                           return (findWithDefault val r bypass)
                   else return val

findWithDefault :: (Eq a) => b -> a -> [ (a, b) ] -> b
findWithDefault def x []            = def
findWithDefault def x ((x', y) : l)
  | x == x'                         = y
  | otherwise                       = findWithDefault def x l
