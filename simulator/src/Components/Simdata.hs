module Components.Simdata
  ( Simdata(..)
  ) where

data Simdata = Simdata
  { cycles                     :: Int
  -- Counts the number of instrctions executed.
  , insts                      :: Int
  -- Counts the maximum number of instructions executed in a cycle during
  -- execution.
  , max_insts_per_cycle        :: Int
  -- Counts the number of cycles each stage is stalled during execution.
  , fetch_stalled_count        :: Int
  , decode_stalled_count       :: Int
  , issue_stalled_count        :: Int
  , execute_stalled_count      :: Int
  , writeback_stalled_count    :: Int
  -- Counts the number of correct and incorrect branch predictions.
  , correct_predictions        :: Int
  , incorrect_predictions      :: Int
  -- Counts the maximum number of instructions executed out of order in a cycle
  -- during execution.
  , max_out_of_order_per_cycle :: Int
  }
