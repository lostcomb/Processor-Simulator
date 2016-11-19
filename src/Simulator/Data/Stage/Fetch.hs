module Simulator.Data.Stage.Fetch
  ( Fetch(..)
  , newFetch
  ) where

import Data.Word

data Fetch = Fetch
  { _fetchStalled        :: Bool
  , _fetchInvalidated    :: Bool
  , _fetchProgramCounter :: Word32
  }
  deriving (Show, Eq, Read)

newFetch :: Fetch
newFetch = Fetch
  { _fetchStalled        = False
  , _fetchInvalidated    = False
  , _fetchProgramCounter = 0
  }
