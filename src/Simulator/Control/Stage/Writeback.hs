module Simulator.Control.Stage.Writeback
  ( writeback
  ) where

import Data.Int
import Control.Lens
import Control.Monad.State

import Simulator.Data.Processor

writeback :: [ Maybe (Register, Int32) ] -> State Processor ()
writeback input = do isStalled <- use $ writebackStage.stalled
                     if isStalled
                       then return ()
                       else mapM_ writeback' input

writeback' :: Maybe (Register, Int32) -> State Processor ()
writeback' Nothing = return ()
writeback' (Just (r, v)) = do regFile.regVal r .= v
                              regFile.regFlag r .= Clean
