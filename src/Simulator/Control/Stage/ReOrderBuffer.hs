module Simulator.Control.Stage.ReOrderBuffer
  ( superscalarReOrderBuffer
  ) where

import Simulator.Data.Processor

superscalarReOrderBuffer :: ([ DecodedData ], [ ExecutedData ]) -> ProcessorState ([ DecodedData ], [ ExecutedData ])
superscalarReOrderBuffer = undefined
{- TODO: Make space available for the decoded data, and put the executed data
   into the reorder buffer. If there are any items in the re-order buffer
   available to be written to the register file, then do so.
   This must handle invalidation. -}
