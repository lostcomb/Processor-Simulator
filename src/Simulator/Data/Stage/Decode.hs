module Simulator.Data.Stage.Decode
  ( Decode(..)
  , newDecode
  ) where

data Decode = Decode
  { _decodeStalled     :: Bool
  , _decodeSpeculative :: Bool
  }
  deriving (Show, Eq, Read)

newDecode :: Decode
newDecode = Decode
  { _decodeStalled     = False
  , _decodeSpeculative = False
  }
