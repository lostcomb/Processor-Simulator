module Simulator.Control.Stage.Writeback
  ( writeback
  ) where

import Data.Int
import Control.Monad.State

import Data.Stage
import Data.Registers
import Data.Processor
import Data.Instruction

writeback :: (Processor p, RegisterFile p) => [ Maybe (Register, Int32) ] -> State p ()
writeback = do w <- getWriteback
               if stalled w
                 then return ()
                 else mapM_ writeback'

writeback' :: (Processor p, RegisterFile p) => Maybe (Register, Int32) -> State p ()
writeback' Nothing = return ()
writeback' (Just (r, v)) = setReg r v
