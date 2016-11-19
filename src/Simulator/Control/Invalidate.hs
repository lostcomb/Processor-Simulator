module Simulator.Control.Invalidate
  ( invalidateFetch
  , validateFetch
  , invalidateDecode
  , validateDecode
  , invalidateIssue
  , validateIssue
  , invalidateExecute
  , validateExecute
  , invalidateWriteback
  , validateWriteback
  ) where

import Control.Lens
import Control.Monad.State

import Simulator.Data.Processor

-- |This function invalidates the fetch stage of the processor.
invalidateFetch :: ProcessorState ()
invalidateFetch = fetchStage.invalidated .= True

-- |This function validates the fetch stage and all stages after it in the
--  pipeline.
validateFetch :: ProcessorState ()
validateFetch = fetchStage.invalidated .= False

-- |This function invalidates the decode stage of the processor and all stages
--  before it in the pipeline.
invalidateDecode :: ProcessorState ()
invalidateDecode = do invalidateFetch
                      decodeStage.invalidated .= True

-- |This function validates the decode stage and all stages after it in the
--  pipeline.
validateDecode :: ProcessorState ()
validateDecode = decodeStage.invalidated .= False

-- |This function invalidates the issue stage of the processor and all stages
--  before it in the pipeline.
invalidateIssue :: ProcessorState ()
invalidateIssue = do invalidateDecode
                     issueStage.invalidated .= True

-- |This function validates the issue stage and all stages after it in the
--  pipeline.
validateIssue :: ProcessorState ()
validateIssue = issueStage.invalidated .= False

-- |This function invalidates the execute stage of the processor and all stages
--  before it in the pipeline.
invalidateExecute :: ProcessorState ()
invalidateExecute = do invalidateIssue
                       executeStage.invalidated .= True

-- |This function validates the execute stage and all stages after it in the
--  pipeline.
validateExecute :: ProcessorState ()
validateExecute = executeStage.invalidated .= False

-- |This function invalidates the writeback stage of the processor and all stages
--  before it in the pipeline.
invalidateWriteback :: ProcessorState ()
invalidateWriteback = do invalidateExecute
                         writebackStage.invalidated .= True

-- |This function validates the writeback stage.
validateWriteback :: ProcessorState ()
validateWriteback = writebackStage.invalidated .= False
