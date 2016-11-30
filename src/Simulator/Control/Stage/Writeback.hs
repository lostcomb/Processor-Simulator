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
                           Just (r, v, inv) -> do
                             regFile.regVal  r  .= v
                             regFile.regFlag r  -= 1
                             when inv $ do
                               fetchStage.programCounter .= fromIntegral v
                               invalidate .= True
                               cleanRegisters
                           Nothing     -> return ()

cleanRegisters :: ProcessorState ()
cleanRegisters = mapM_ (\r -> regFile.regFlag r .= clean) [(minBound::Register)..]
