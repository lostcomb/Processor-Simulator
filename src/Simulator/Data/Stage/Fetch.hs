module Simulator.Data.Stage.Fetch
  ( Fetch(..)
  , newFetch
  ) where

import Data.Word

data Fetch = Fetch
  { _fetchStalled        :: Bool
  , _fetchNoOfInsts      :: Int
  , _fetchProgramCounter :: Word32
  }
  deriving (Show, Eq, Read)

newFetch :: Int -> Fetch
newFetch n = Fetch
  { _fetchStalled        = False
  , _fetchNoOfInsts      = n
  , _fetchProgramCounter = 0
  }
