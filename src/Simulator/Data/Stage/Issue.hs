module Simulator.Data.Stage.Issue
  ( Issue(..)
  , newIssue
  ) where

import Simulator.Data.Stall

data Issue = Issue
  { _issueStalled     :: Stalled
  , _issueSpeculative :: Bool
  }
  deriving (Show, Eq, Read)

newIssue :: Issue
newIssue = Issue
  { _issueStalled     = newStalled
  , _issueSpeculative = False
  }
