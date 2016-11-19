module Simulator.Control.Stage.Writeback
  ( scalarWriteback
  , pipelinedWriteback
  ) where

import Data.Int
import Control.Lens
import Control.Monad.State

import Simulator.Data.Processor

scalarWriteback :: [ Maybe (Register, Int32) ] -> ProcessorState ()
scalarWriteback input = do regFile.regVal pc += instLength
                           mapM_ writeback input

pipelinedWriteback :: [ Maybe (Register, Int32) ] -> ProcessorState ()
pipelinedWriteback input = condM (use $ writebackStage.stalled)
  (simData.writebackStalledCount += 1 >> return ()) $
  do mapM_ writeback input

writeback :: Maybe (Register, Int32) -> ProcessorState ()
writeback Nothing = return ()
writeback (Just (r, v)) = do regFile.regVal r .= v
                             regFile.regFlag r .= Clean
