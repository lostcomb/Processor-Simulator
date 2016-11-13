module Simulator.Data.Stage.Execute
  ( Execute(..)
  , newExecute
  ) where

import Data.Int
import Simulator.Data.Registers

data Execute = Execute
  { _executeStalled :: Bool
  , _noOfEUs        :: Int
  , _bypassValues   :: [ (Register, Int32) ]
  }
  deriving (Show, Eq, Read)

newExecute :: Int -> Execute
newExecute n = Execute
  { _executeStalled = False
  , _noOfEUs        = n
  , _bypassValues   = []
  }
