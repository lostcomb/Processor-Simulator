module Simulator.Control.Stall
  ( stallFetch
  , continueFetch
  , stallDecode
  , continueDecode
  , stallIssue
  , continueIssue
  , stallExecute
  , continueExecute
  , stallWriteback
  , continueWriteback
  ) where

import Control.Lens
import Control.Monad.State

import Simulator.Data.Processor

-- |This function stalls the fetch stage of the processor.
stallFetch :: ProcessorState ()
stallFetch = fetchStage.stalled .= True

-- |This function continues the fetch stage and all stages after it in the
--  pipeline.
continueFetch :: ProcessorState ()
continueFetch = do fetchStage.stalled .= False
                   continueDecode

-- |This function stalls the decode stage of the processor and all stages
--  before it in the pipeline.
stallDecode :: ProcessorState ()
stallDecode = do stallFetch
                 decodeStage.stalled .= True

-- |This function continues the decode stage and all stages after it in the
--  pipeline.
continueDecode :: ProcessorState ()
continueDecode = do decodeStage.stalled .= False
                    continueIssue

-- |This function stalls the issue stage of the processor and all stages
--  before it in the pipeline.
stallIssue :: ProcessorState ()
stallIssue = do stallDecode
                issueStage.stalled .= True

-- |This function continues the issue stage and all stages after it in the
--  pipeline.
continueIssue :: ProcessorState ()
continueIssue = do issueStage.stalled .= False
                   continueExecute

-- |This function stalls the execute stage of the processor and all stages
--  before it in the pipeline.
stallExecute :: ProcessorState ()
stallExecute = do stallIssue
                  executeStage.stalled .= True

-- |This function continues the execute stage and all stages after it in the
--  pipeline.
continueExecute :: ProcessorState ()
continueExecute = do executeStage.stalled .= False
                     continueWriteback

-- |This function stalls the writeback stage of the processor and all stages
--  before it in the pipeline.
stallWriteback :: ProcessorState ()
stallWriteback = do stallExecute
                    writebackStage.stalled .= True

-- |This function continues the writeback stage.
continueWriteback :: ProcessorState ()
continueWriteback = do writebackStage.stalled .= False
