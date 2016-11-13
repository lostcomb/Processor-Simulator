module Simulator.Data.Stallable
  ( Stallable(..)
  ) where

import Control.Lens

class Stallable s where
  stalled :: Lens' s Bool
