module Simulator.Data.Stage.Decode
  ( Decode(..)
  , newDecode
  ) where

import Simulator.Data.Stall

data Decode = Decode
  { _decodeStalled     :: Stalled
  , _decodeSpeculative :: Bool
  }
  deriving (Show, Eq, Read)

newDecode :: Decode
newDecode = Decode
  { _decodeStalled     = newStalled
  , _decodeSpeculative = False
  }
