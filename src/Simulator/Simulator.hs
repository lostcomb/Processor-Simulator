module Simulator.Simulator
  ( runProcessor
  ) where

import Control.Lens
import Control.Monad.State

import Simulator.Data.Processor
import Simulator.Control.Stage.Fetch
import Simulator.Control.Stage.Decode
import Simulator.Control.Stage.Issue
import Simulator.Control.Stage.Execute
import Simulator.Control.Stage.Writeback

runProcessor :: ProcessorState ()
runProcessor = do pt <- use $ options.procType
                  if pt == Scalar
                    then scalarProcessor
                  else if pt == Pipelined
                    then pipelinedProcessor
                  else superscalarProcessor

scalarProcessor :: ProcessorState ()
scalarProcessor = whileM_ (use halted) $
  do f <- scalarFetch
     d <- scalarDecode f
     i <- scalarIssue d
     e <- scalarExecute i
     w <- scalarWriteback e
     simData.cycles += 5

pipelinedProcessor :: ProcessorState ()
pipelinedProcessor = whileM_ (use halted) $
  do f <- pipelinedFetch
     d <- pipelinedDecode f
     i <- pipelinedIssue d
     e <- pipelinedExecute i
     w <- pipelinedWriteback e
     decInputLatches .= f
     issInputLatches .= d
     exeInputLatches .= i
     wrbInputLatches .= e
     simData.cycles += 1

superscalarProcessor :: ProcessorState ()
superscalarProcessor = undefined
