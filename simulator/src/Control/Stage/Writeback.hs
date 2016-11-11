module Control.Stage.Writeback
  ( writeback
  ) where

import Data.Int
import Control.Monad.State

import Data.Stage
import Data.Registers
import Data.Processor
import Data.Instruction

writeback :: (Processor p, RegisterFile p) => [ Maybe (Register, Int32) ] -> State p ()
writeback = mapM_ writeback'

writeback' :: (Processor p, RegisterFile p) => Maybe (Register, Int32) -> State p ()
writeback' Nothing = do w <- getWriteback
                        setWriteback $ stall w
writeback' (Just (r, v)) = do w <- getWriteback
                              setWriteback $ continue w
                              setReg r v
