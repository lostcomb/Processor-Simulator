module Simulator.Control.Stage.Fetch
  ( fetch
  ) where

import Data.Word
import Control.Lens
import Control.Monad
import Control.Monad.State

import Simulator.Data.Processor

fetch :: State Processor [ Maybe (Word8, Word8, Word8, Word8) ]
fetch = do n_insts <- use $ fetchStage.noOfInsts
           isStalled <- use $ fetchStage.stalled
           if isStalled
             then use decInputLatches >>= return
             else replicateM n_insts fetch'

fetch' :: State Processor (Maybe (Word8, Word8, Word8, Word8))
fetch' = do pc_val <- use $ fetchStage.programCounter
            i1 <- use $ instMem.item (fromIntegral  pc_val     )
            i2 <- use $ instMem.item (fromIntegral (pc_val + 1))
            i3 <- use $ instMem.item (fromIntegral (pc_val + 2))
            i4 <- use $ instMem.item (fromIntegral (pc_val + 3))
            fetchStage.programCounter += 4
            return $ Just (i1, i2, i3, i4)
