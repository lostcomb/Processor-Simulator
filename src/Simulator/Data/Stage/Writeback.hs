module Simulator.Data.Stage.Writeback
  ( Writeback
  , newWriteback
  ) where

import Control.Lens
import Simulator.Data.Stallable

data Writeback = Writeback
  { _stalled :: Bool
  }
  deriving (Show, Eq, Read)

newWriteback :: Writeback
newWriteback = Writeback
  { _stalled = False
  }

instance Stallable Writeback where
  stalled = lens _stalled (\wrb s -> wrb { _stalled = s })
