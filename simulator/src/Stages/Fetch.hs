module Stages.Fetch
  ( fetch
  ) where

import Data.Word
import Components.Registers
import Components.ProcessorState

fetch :: ProcessorState -> (Word32, ProcessorState)
fetch s = (instruction, setReg pc (pc_val + 1) s)
  where pc_val      = getReg pc s
        instruction = getInstruction pc_val s
