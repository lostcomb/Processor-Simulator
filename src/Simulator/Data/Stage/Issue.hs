module Data.Stage.Issue
  ( Issue
  , newIssue
  , issueWindow
  , execWindow
  ) where

import Control.Lens
import Data.Stallable
import Data.Instruction

data Issue = Issue
  { _stalled      :: Bool
  , _issueWindow :: [ InstructionReg ]
  , _execWindow  :: [ InstructionReg ]
  }
  deriving (Show, Eq, Read)

newIssue :: Issue
newIssue = Issue
  { _stalled     = False
  , _issueWindow = []
  , _execWindow  = []
  }

instance Stallable Issue where
  stalled = lens _stalled (\iss s -> iss { _stalled = s })

issueWindow :: Lens' Issue [ InstructionReg ]
issueWindow = lens _issueWindow (\iss i -> iss { _issueWindow = i })

execWindow :: Lens' Issue [ InstructionReg ]
execWindow = lens _execWindow (\iss e -> iss { execWindow = e })
