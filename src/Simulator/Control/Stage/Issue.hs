module Simulator.Control.Stage.Issue
  ( issue
  ) where

import Control.Monad.State

import Data.Stage
import Data.Processor
import Data.Instruction

issue :: (Processor p, Issue p) => [ Maybe InstructionReg ] -> State p [ Maybe InstructionVal ]
issue = do i <- getIssue
           if stalled i
             then do output <- getExeInputLatches
                     return output
             else mapM issue'

issue' :: (Processor p, Issue p) => Maybe InstructionReg -> State p (Maybe InstructionVal)
issue' Nothing = return Nothing
issue' (Just i) = undefined --TODO: Take account of instruction dependencies here and use
                            -- bypassing if available.
