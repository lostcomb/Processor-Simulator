module Simulator.Data.Stage.Fetch
  ( Fetch
  , newFetch
  , noOfInsts
  , programCounter
  ) where

import Data.Word
import Control.Lens
import Simulator.Data.Stallable

data Fetch = Fetch
  { _stalled         :: Bool
  , _noOfInsts     :: Int
  , _programCounter :: Word32
  }
  deriving (Show, Eq, Read)

newFetch :: Int -> Fetch
newFetch n = Fetch
  { _stalled        = False
  , _noOfInsts      = n
  , _programCounter = 0
  }

instance Stallable Fetch where
  stalled = lens _stalled (\fet s -> fet { _stalled = s })

noOfInsts :: Lens' Fetch Int
noOfInsts = lens _noOfInsts (\fet n -> fet { _noOfInsts = n })

programCounter :: Lens' Fetch Word32
programCounter = lens _programCounter (\fet pc -> fet { _programCounter = pc })
