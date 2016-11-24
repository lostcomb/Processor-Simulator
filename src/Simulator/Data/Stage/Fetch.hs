module Simulator.Data.Stage.Fetch
  ( Fetch(..)
  , newFetch
  ) where

import Data.Word
import Simulator.Data.Stall

data Fetch = Fetch
  { _fetchStalled        :: Stalled
  , _fetchProgramCounter :: Word32
  }
  deriving (Show, Eq, Read)

newFetch :: Fetch
newFetch = Fetch
  { _fetchStalled        = newStalled
  , _fetchProgramCounter = 0
  }
