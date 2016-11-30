module Simulator.Control.Stage.Writeback
  ( scalarWriteback
  , pipelinedWriteback
  , superscalarWriteback
  ) where

import Data.Int
import Data.Word
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
writeback (Just  d) = do regFile.regVal pc += instLength
                         case d of
                           (Just (r, v), inv) -> do
                             regFile.regVal  r .= v
                             regFile.regFlag r -= 1
                             checkForInvalidation inv $ fromIntegral v
                           (Nothing    , inv) -> do
                             pc_val <- use $ regFile.regVal pc
                             regFile.regFlag pc -= 1
                             checkForInvalidation inv $ fromIntegral pc_val

checkForInvalidation :: Bool -> Word32 -> ProcessorState ()
checkForInvalidation inv v = when inv $ do
  fetchStage.programCounter .= v
  invalidate .= True
  cleanRegisters

cleanRegisters :: ProcessorState ()
cleanRegisters = mapM_ (\r -> regFile.regFlag r .= clean) [(minBound::Register)..]
