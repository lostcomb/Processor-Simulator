module Simulator.Control.Stage.Writeback
  ( scalarWriteback
  , pipelinedWriteback
  , superscalarWriteback
  ) where

import Data.Int
import Control.Lens
import Control.Monad
import Control.Monad.State

import Simulator.Data.Processor
import Simulator.Control.Stall

scalarWriteback :: ExecutedData -> ProcessorState ()
scalarWriteback = writeback

pipelinedWriteback :: ExecutedData -> ProcessorState ()
pipelinedWriteback = writeback

superscalarWriteback :: [ ExecutedData ] -> ProcessorState ()
superscalarWriteback = undefined

writeback :: ExecutedData -> ProcessorState ()
writeback (Nothing) = return ()
writeback (Just  d) = do regFile.regVal  pc += instLength
                         case d of
                           Just (r, v) -> do
                             regFile.regVal  r  .= v
                             regFile.regFlag r  .= Clean
                             when (r == pc) $ do
                               fetchStage.programCounter .= fromIntegral v
                               invalidate .= True
                           Nothing     -> return ()
