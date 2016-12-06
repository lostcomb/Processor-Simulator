module Simulator.Data.Stage.ReOrderBuffer
  ( ReOrderBuffer(..)
  , newReOrderBuffer
  ) where

import Data.Int

import Simulator.Data.Stall
import Simulator.Data.Registers

data ReOrderBuffer = ReOrderBuffer
  { _reOrderBufferStalled :: Stalled
  , _reOrderBufferBuffer  :: [ (Register, Maybe Int32, Bool) ]
  }

newReOrderBuffer :: ReOrderBuffer
newReOrderBuffer = ReOrderBuffer
  { _reOrderBufferStalled = newStalled
  , _reOrderBufferBuffer  = []
  }
