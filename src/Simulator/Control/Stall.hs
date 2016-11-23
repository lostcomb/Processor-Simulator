{-# LANGUAGE FlexibleContexts, RankNTypes #-}
module Simulator.Control.Stall
  ( module Simulator.Control.Stall
  ) where

import Control.Lens
import Control.Monad.State

import Simulator.Data.Processor

-- |This function returns the value of a Left.
leftM :: (Monad m) => (Either a b) -> m a
leftM (Left a) = return a
leftM _        = fail "leftM applied to Right value."

-- |This function returns the value of a Right.
rightM :: (Monad m) => (Either a b) -> m b
rightM (Right b) = return b
rightM _         = fail "rightM applied to Left value."

-- |This function returns the Left value of checkStall.
checkStallP :: (HasStalled a Bool)
            => Lens' Processor a
            -> Lens' Simdata Int
            -> ProcessorState (Either b c)
            -> ProcessorState (Either b c)
            -> ProcessorState b
checkStallP stage count latches m = checkStall stage count latches m >>= leftM

-- |This function returns the Right value of checkStall.
checkStallS :: (HasStalled a Bool)
            => Lens' Processor a
            -> Lens' Simdata Int
            -> ProcessorState (Either b c)
            -> ProcessorState (Either b c)
            -> ProcessorState c
checkStallS stage count latches m = checkStall stage count latches m >>= rightM

-- |This function returns @latches@ and increments @count@ if @stage@ is stalled.
--  If @stage@ is not stalled, it returns @m@.
checkStall :: (HasStalled a Bool)
           => Lens' Processor a
           -> Lens' Simdata Int
           -> ProcessorState b
           -> ProcessorState b
           -> ProcessorState b
checkStall stage count latches m
  = do s <- use $ stage.stalled
       if s then do
         simData.count += 1
         latches
       else m

-- |This function continues all of the stages of the processor.
continueAll :: ProcessorState ()
continueAll = do continueFetch
                 continueDecode
                 continueIssue
                 continueExecute
                 continueWriteback

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
