module Stages.Fetch
  ( fetch
  ) where

import Data.Word
import Components.Registers
import Components.Processor
import Control.Monad.State

fetch :: State Processor (Word8, Word8, Word8, Word8)
fetch = do pc_val <- getReg pc
           i1 <- getInstruction (fromIntegral pc_val)
           i2 <- getInstruction (fromIntegral (pc_val + 1))
           i3 <- getInstruction (fromIntegral (pc_val + 2))
           i4 <- getInstruction (fromIntegral (pc_val + 3))
           setReg pc (pc_val + 4)
           updateCycles 1
           return (i1, i2, i3, i4)
