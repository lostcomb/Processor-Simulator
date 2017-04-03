{-
This file is part of aca-processor-simulator.

aca-processor-simulator is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

aca-processor-simulator is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with aca-processor-simulator.  If not, see <http://www.gnu.org/licenses/>.
-}
module Simulator.Data.Stage.ReOrderBuffer
  ( module Simulator.Data.Stage.ReOrderBuffer
  ) where

import Data.Int

import Simulator.Data.Stall
import Simulator.Data.Registers
import Simulator.Data.Instruction

-- |This type defines a reorder buffer entry. The types correspond to:
--  (id, type, destination, value, invalidate, valid, completed).
type ReOrderBufferEntry = (Int, InstType, Register, Maybe Int32, Bool, Bool, Bool)

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
