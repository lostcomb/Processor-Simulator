module Simulator.Data.Stage.Decode
  ( Decode
  , newDecode
  ) where

import Control.Lens
import Simulator.Data.Stallable

data Decode = Decode
  { _stalled :: Bool
  }
  deriving (Show, Eq, Read)

newDecode :: Decode
newDecode = Decode
  { _stalled = False
  }

instance Stallable Decode where
  stalled = lens _stalled (\dec s -> dec { _stalled = s })
