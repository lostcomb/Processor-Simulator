module Simulator.Data.Stage.Writeback
  ( Writeback(..)
  , newWriteback
  ) where

import Simulator.Data.Stall

data Writeback = Writeback
  { _writebackStalled :: Stalled
  }
  deriving (Show, Eq, Read)

newWriteback :: Writeback
newWriteback = Writeback
  { _writebackStalled = newStalled
  }
