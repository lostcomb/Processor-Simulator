module Control.Stage.Issue
  ( issue
  ) where

import Control.Monad.State

import Data.Stage
import Data.Processor
import Data.Instruction

issue :: (Processor p, Issue p) => [ Maybe InstructionReg ] -> State p [ Maybe InstructionVal ]
issue = mapM issue'

issue' :: (Processor p, Issue p) => Maybe InstructionReg -> State p (Maybe InstructionVal)
issue' Nothing = do i <- getIssue
                    setIssue $ stall i
                    return Nothing
issue' (Just i) = undefined --TODO: Take account of instruction dependencies here and use
                            -- bypassing if available.
