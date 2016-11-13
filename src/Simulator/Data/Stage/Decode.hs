module Simulator.Data.Stage.Decode
  ( Decode(..)
  , newDecode
  ) where

data Decode = Decode
  { _decodeStalled :: Bool
  }
  deriving (Show, Eq, Read)

newDecode :: Decode
newDecode = Decode
  { _decodeStalled = False
  }
