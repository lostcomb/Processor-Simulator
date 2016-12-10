module Simulator.Data.Stage.ReOrderBuffer
  ( module Simulator.Data.Stage.ReOrderBuffer
  ) where

import Data.Int

import Simulator.Data.Stall
import Simulator.Data.Registers
import Simulator.Data.Instruction

-- |This type defines a reorder buffer entry.
type ReOrderBufferEntry = (Int, InstType, Register, Maybe Int32, Bool, Bool)

-- |This data type defines a reorder buffer.
data ReOrderBuffer = ReOrderBuffer
  { _reOrderBufferStalled :: Stalled
  , _reOrderBufferBuffer  :: [ ReOrderBufferEntry ]
  , _reOrderBufferNextId  :: Int
  }

-- |This defines an empty reorder buffer.
newReOrderBuffer :: ReOrderBuffer
newReOrderBuffer = ReOrderBuffer
  { _reOrderBufferStalled = newStalled
  , _reOrderBufferBuffer  = []
  , _reOrderBufferNextId  = 0
  }
