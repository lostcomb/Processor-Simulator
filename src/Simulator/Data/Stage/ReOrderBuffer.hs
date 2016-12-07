module Simulator.Data.Stage.ReOrderBuffer
  ( module Simulator.Data.Stage.ReOrderBuffer
  ) where

import Data.Int

import Simulator.Data.Stall
import Simulator.Data.Registers

-- |This data type defines a reorder buffer.
data ReOrderBuffer = ReOrderBuffer
  { _reOrderBufferStalled :: Stalled
  , _reOrderBufferBuffer  :: [ (Register, Maybe Int32, Bool) ]
  }

-- |This defines an empty reorder buffer.
newReOrderBuffer :: ReOrderBuffer
newReOrderBuffer = ReOrderBuffer
  { _reOrderBufferStalled = newStalled
  , _reOrderBufferBuffer  = []
  }
