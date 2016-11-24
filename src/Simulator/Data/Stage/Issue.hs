module Simulator.Data.Stage.Issue
  ( Issue(..)
  , newIssue
  ) where

import Simulator.Data.Stall
import Simulator.Data.Instruction

data Issue = Issue
  { _issueStalled     :: Stalled
  , _issueIssueWindow :: [ InstructionReg ]
  , _issueExecWindow  :: [ InstructionReg ]
  }
  deriving (Show, Eq, Read)

newIssue :: Issue
newIssue = Issue
  { _issueStalled     = newStalled
  , _issueIssueWindow = []
  , _issueExecWindow  = []
  }
