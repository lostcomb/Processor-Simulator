module Data.Stage.Execute
  ( Execute
  , newExecute
  , noOfEUs
  , bypassValues
  ) where

import Data.Int
import Control.Lens
import Data.Stallable
import Data.Registers

data Execute = Execute
  { _stalled      :: Bool
  , _noOfEUs      :: Int
  , _bypassValues :: [ (Register, Int32) ]
  }
  deriving (Show, Eq, Read)

newExecute :: Int -> Execute
newExecute n = Execute
  { _stalled      = False
  , _noOfEUs      = n
  , _bypassValues = []
  }

instance Stallable Execute where
  stalled = lens _stalled (\exe s -> exe { _stalled = s })

noOfEUs :: Lens' Execute Int
noOfEUs = lens _noOfEUs (\exe u -> exe { _noOfEUs = u })

bypassValues :: Lens' Execute [ (Register, Int32) ]
bypassValues = lens _bypassValues (\exe b -> exe { _bypassValues = b })
