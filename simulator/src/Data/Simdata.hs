module Data.Simdata
  ( Simdata(..)
  ) where

import Data.Instruction

data Simdata = Simdata
  -- Counts the number of cycles executed.
  { cycles                  :: Int
  -- Counts the number of instrctions executed.
  , insts                   :: Int
  -- Counts the number of instructions executed in each cycle during execution.
  , insts_per_cycle         :: [ Int ]
  -- Counts the number of cycles each stage is stalled during execution.
  , fetch_stalled_count     :: Int
  , decode_stalled_count    :: Int
  , issue_stalled_count     :: Int
  , execute_stalled_count   :: Int
  , writeback_stalled_count :: Int
  -- Counts the number of correct and incorrect branch predictions.
  , correct_predictions     :: [ InstructionReg ]
  , incorrect_predictions   :: [ InstructionReg ]
  -- Counts the number of instructions executed out of order in each cycle
  -- during execution.
  , out_of_order_per_cycle  :: [ Int ]
  }
