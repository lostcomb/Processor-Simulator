module Simulator.Data.Stage.Execute
  ( Execute(..)
  , newExecute
  ) where

import Data.Int
import Simulator.Data.Registers

type Latches = [ Maybe (Register, Int32) ]

data Execute = Execute
  { _executeStalled         :: Bool
  , _executeBypassValues    :: [ (Register, Int32) ]
  , _executePipelineLatches :: [ Latches ]
  }
  deriving (Show, Eq, Read)

newExecute :: Execute
newExecute = Execute
  { _executeStalled         = False
  , _executeBypassValues    = []
  , _executePipelineLatches = []
  }
