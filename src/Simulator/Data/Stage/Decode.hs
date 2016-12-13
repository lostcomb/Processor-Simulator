module Simulator.Data.Stage.Decode
  ( Decode(..)
  , newDecode
  ) where

import Simulator.Data.Stall

data Decode = Decode
  { _decodeStalled :: Stalled
  }
  deriving (Show, Eq, Read)

newDecode :: Decode
newDecode = Decode
  { _decodeStalled = newStalled
  }
