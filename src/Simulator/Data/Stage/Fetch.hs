module Simulator.Data.Stage.Fetch
  ( Fetch(..)
  , newFetch
  ) where

import Data.Word

data Fetch = Fetch
  { _fetchStalled   :: Bool
  , _noOfInsts      :: Int
  , _programCounter :: Word32
  }
  deriving (Show, Eq, Read)

newFetch :: Int -> Fetch
newFetch n = Fetch
  { _fetchStalled   = False
  , _noOfInsts      = n
  , _programCounter = 0
  }
