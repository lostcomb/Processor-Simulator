module Data.Stage
  ( Stage(..)
  ) where

class Stage s where
  stall    :: s -> s
  continue :: s -> s
  stalled  :: s -> Bool
