module Data.Stage.Writeback
  ( Writeback_T(..)
  , newWriteback
  ) where

import Data.Stage

data Writeback_T = Writeback_T
  { _stalled :: Bool
  }
  deriving (Show, Eq, Read)

instance Stage Writeback_T where
  stall    w = w { _stalled = True  }
  continue w = w { _stalled = False }
  stalled  w = _stalled w

newWriteback :: Writeback_T
newWriteback = Writeback_T
  { _stalled = False
  }
