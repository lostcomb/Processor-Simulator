module Data.Stage.Decode
  ( Decode_T(..)
  , Decode(..)
  , newDecode
  ) where

import Data.Stage
import Data.Registers
import Data.Instruction
import Control.Monad.State

data Decode_T = Decode_T
  { _stalled :: Bool
  }
  deriving (Show, Eq, Read)

instance Stage Decode_T where
  stall    d = d { _stalled = True  }
  continue d = d { _stalled = False }
  stalled  d = _stalled d

newDecode :: Decode_T
newDecode = Decode_T
  { _stalled = False
  }

class Decode d where
  getInstructionCycles :: Inst Register -> State d Int
