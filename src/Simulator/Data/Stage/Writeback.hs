module Simulator.Data.Stage.Writeback
  ( Writeback(..)
  , newWriteback
  ) where

import Simulator.Data.Stall

data Writeback = Writeback
  { _writebackStalled   :: Stalled
  , _writebackHaltAfter :: Maybe Int
  }
  deriving (Show, Eq, Read)

newWriteback :: Writeback
newWriteback = Writeback
  { _writebackStalled   = newStalled
  , _writebackHaltAfter = Nothing
  }
