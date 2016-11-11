module Data.Stage.Issue
  ( Issue_T(..)
  , Issue(..)
  , newIssue
  ) where

import Data.Stage
import Data.Instruction
import Control.Monad.State

data Issue_T = Issue_T
  { _stalled      :: Bool
  , _issue_window :: [ InstructionReg ]
  , _exec_window  :: [ InstructionReg ]
  }
  deriving (Show, Eq, Read)

instance Stage Issue_T where
  stall    i = i { _stalled = True  }
  continue i = i { _stalled = False }
  stalled  i = _stalled i

newIssue :: Issue_T
newIssue = Issue_T
  { _stalled      = False
  , _issue_window = []
  , _exec_window  = []
  }

class Issue i where
  getIssueWindow :: State i [ InstructionReg ]
  setIssueWindow :: [ InstructionReg ] -> State i ()
  getExecWindow  :: State i [ InstructionReg ]
  setExecWindow  :: [ InstructionReg ] -> State i ()
