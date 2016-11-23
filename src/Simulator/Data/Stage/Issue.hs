module Simulator.Data.Stage.Issue
  ( Issue(..)
  , newIssue
  ) where

import Simulator.Data.Instruction

data Issue = Issue
  { _issueStalled      :: Bool
  , _issueIssueWindow  :: [ InstructionReg ]
  , _issueExecWindow   :: [ InstructionReg ]
  }
  deriving (Show, Eq, Read)

newIssue :: Issue
newIssue = Issue
  { _issueStalled      = False
  , _issueIssueWindow  = []
  , _issueExecWindow   = []
  }
