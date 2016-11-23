module Simulator.Control.Stage.Fetch
  ( scalarFetch
  , pipelinedFetch
  , superscalarFetch
  ) where

import Data.Word
import Control.Lens
import Control.Monad
import Control.Monad.State

import Simulator.Data.Processor
import Simulator.Control.Stall

scalarFetch :: ProcessorState FetchedData
scalarFetch = fetch

pipelinedFetch :: ProcessorState FetchedData
pipelinedFetch = fetch

superscalarFetch :: ProcessorState [ FetchedData ]
superscalarFetch = undefined

fetch :: ProcessorState FetchedData
fetch = do pc_val <- use $ fetchStage.programCounter
           i1 <- use $ instMem.item (fromIntegral  pc_val     )
           i2 <- use $ instMem.item (fromIntegral (pc_val + 1))
           i3 <- use $ instMem.item (fromIntegral (pc_val + 2))
           i4 <- use $ instMem.item (fromIntegral (pc_val + 3))
           fetchStage . programCounter += instLength
           return $ Just (i1, i2, i3, i4)
