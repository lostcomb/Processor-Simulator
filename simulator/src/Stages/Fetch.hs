module Stages.Fetch
  ( fetch
  ) where

import Data.Word
import Components.Registers
import Components.ProcessorState

fetch :: ProcessorState -> ((Word8, Word8, Word8, Word8), ProcessorState)
fetch s = (instruction, updateCycles 1 $ setReg pc (pc_val + 4) s)
  where pc_val      = getReg pc s
        instruction = ( getInstruction  pc_val      s
                      , getInstruction (pc_val + 1) s
                      , getInstruction (pc_val + 2) s
                      , getInstruction (pc_val + 3) s
                      )
