module Simulator.Data.Stage.Writeback
  ( Writeback(..)
  , newWriteback
  ) where

data Writeback = Writeback
  { _writebackStalled     :: Bool
  , _writebackInvalidated :: Bool
  }
  deriving (Show, Eq, Read)

newWriteback :: Writeback
newWriteback = Writeback
  { _writebackStalled     = False
  , _writebackInvalidated = False
  }
