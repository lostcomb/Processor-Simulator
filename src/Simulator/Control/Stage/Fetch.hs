module Simulator.Control.Stage.Fetch
  ( fetch
  ) where

import Data.Word
import Control.Lens

import Simulator.Data.Processor

fetch :: State p [ Maybe (Word8, Word8, Word8, Word8) ]
fetch = do n_insts <- fetchStage.noOfInsts
           stalled? <- fetchStage.stalled
           if stalled?
             then do output <- getDecInputLatches
                     return output
             else mapM (\_ -> do pc_val <- getProgramCounter
                                 i1 <- getInstMemItem (fromIntegral pc_val)
                                 i2 <- getInstMemItem (fromIntegral (pc_val + 1))
                                 i3 <- getInstMemItem (fromIntegral (pc_val + 2))
                                 i4 <- getInstMemItem (fromIntegral (pc_val + 3))
                                 setProgramCounter (pc_val + 4)
                                 return $ Just (i1, i2, i3, i4)) [1..n_insts]
