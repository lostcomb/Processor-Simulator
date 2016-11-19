module Simulator.Control.Stage.Fetch
  ( scalarFetch
  , pipelinedFetch
  ) where

import Data.Word
import Control.Lens
import Control.Monad
import Control.Monad.State

import Simulator.Data.Processor

scalarFetch :: ProcessorState [ Maybe (Word8, Word8, Word8, Word8) ]
scalarFetch = replicateM 1 fetch

pipelinedFetch :: ProcessorState [ Maybe (Word8, Word8, Word8, Word8) ]
pipelinedFetch = condM (use $ fetchStage.stalled) (use decInputLatches) $
  do n_insts <- use $ options.noInstsPerCycle
     replicateM n_insts fetch

fetch :: ProcessorState (Maybe (Word8, Word8, Word8, Word8))
fetch = do pc_val <- use $ fetchStage.programCounter
           i1 <- use $ instMem.item (fromIntegral  pc_val     )
           i2 <- use $ instMem.item (fromIntegral (pc_val + 1))
           i3 <- use $ instMem.item (fromIntegral (pc_val + 2))
           i4 <- use $ instMem.item (fromIntegral (pc_val + 3))
           fetchStage.programCounter += instLength
           return $ Just (i1, i2, i3, i4)
