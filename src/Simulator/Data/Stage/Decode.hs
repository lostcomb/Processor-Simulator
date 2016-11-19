module Simulator.Data.Stage.Decode
  ( Decode(..)
  , newDecode
  ) where

data Decode = Decode
  { _decodeStalled     :: Bool
  , _decodeInvalidated :: Bool
  , _decodeSpeculative :: Bool
  }
  deriving (Show, Eq, Read)

newDecode :: Decode
newDecode = Decode
  { _decodeStalled     = False
  , _decodeInvalidated = False
  , _decodeSpeculative = False
  }
