module Simulator.Control.Stage.Issue
  ( issue
  ) where

import Data.Int
import Control.Lens
import Control.Monad.State

import Simulator.Data.Processor

-- TODO: Finish this module.

issue :: [ Maybe InstructionReg ] -> State Processor [ Maybe InstructionVal ]
issue input = do isStalled <- use $ issueStage.stalled
                 if isStalled
                   then use exeInputLatches >>= return
                   else mapM issue' input

issue' :: Maybe InstructionReg -> State Processor (Maybe InstructionVal)
issue' Nothing = return Nothing
issue' (Just (Instruction c i)) = do inst <- fillInsts i
                                     return $ Just $ Instruction c inst
-- Need to stall if bypassing is not enabled and there is a dependency.

fillInsts :: Inst Register -> State Processor (Inst Int32)
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

getRegVal :: Register -> State Processor Int32
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
