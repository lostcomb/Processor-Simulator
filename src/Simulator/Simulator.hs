{-# LANGUAGE RankNTypes #-}
module Simulator.Simulator
  ( runProcessor
  ) where

import System.IO
import Control.Lens
import Control.Monad (forever)
import Control.Monad.State

import Simulator.Data.Processor
import Simulator.Control.Stage.Fetch
import Simulator.Control.Stage.Decode
import Simulator.Control.Stage.Issue
import Simulator.Control.Stage.Execute
import Simulator.Control.Stage.Writeback
import Simulator.CommandLine.Parser
import Simulator.CommandLine.Command
import Simulator.CommandLine.Interpreter

runProcessor :: StateT [ Command ] (StateT Processor IO) a
runProcessor = do liftIO $ putStrLn "Processor Simulator (c) Julian Loscombe 2016\n"
                  forever $ do liftIO $ putStr "*>"
                               liftIO $ hFlush stdout
                               command_str <- liftIO $ getLine
                               case parse command_str of
                                 (Left   e) -> do put []
                                                  liftIO $ putStrLn e
                                 (Right []) -> do cs <- get
                                                  lift $ interpret step cs
                                 (Right cs) -> do put cs
                                                  lift $ interpret step cs

step :: Type -> ProcessorState ()
step Scalar      = scalarProcessor
step Pipelined   = pipelinedProcessor
step Superscalar = superscalarProcessor

scalarProcessor :: ProcessorState ()
scalarProcessor
  = do f <- scalarFetch
       d <- scalarDecode f
       i <- scalarIssue d
       e <- scalarExecute i
       w <- scalarWriteback e
       simData.cycles += 5

pipelinedProcessor :: ProcessorState ()
pipelinedProcessor
  = do -- Update the stall counts.
       updateStallCounts
       -- Get the value of the latches after the previous cycle.
       (Left d) <- use decInputLatches
       (Left i) <- use issInputLatches
       (Left e) <- use exeInputLatches
       (Left w) <- use wrbInputLatches
       -- Execute the Writeback stage unless it is stalled.
       unless (writebackStage.isStalled) $ do
         pipelinedWriteback w
       -- Execute the Execute stage unless it is stalled.
       unless (executeStage.isStalled) $ do
         e' <- pipelinedExecute e
         case e' of
           (Left    issuedData) -> do exeInputLatches .= Left issuedData
                                      wrbInputLatches .= Left Nothing
           (Right executedData) -> do wrbInputLatches .= Left executedData
       -- Execute the Issue stage unless it is stalled.
       unless (issueStage.isStalled) $ do
         i' <- pipelinedIssue i
         exeInputLatches .= Left i'
       -- Execute the Decode stage unless it is stalled.
       unless (decodeStage.isStalled) $ do
         d' <- pipelinedDecode d
         issInputLatches .= Left d'
       -- Execute the Fetch stage unless it is stalled.
       unless (fetchStage.isStalled) $ do
         f' <- pipelinedFetch
         decInputLatches .= Left f'
       -- Set all latches to Nothing if the pipeline is to be invalidated.
       checkForInvalidation
       -- Increment the number of cycles executed.
       simData.cycles += 1
  where checkForInvalidation :: ProcessorState ()
        checkForInvalidation = do
          i <- use invalidate
          when i $ do
            decInputLatches          .= Left Nothing
            issInputLatches          .= Left Nothing
            exeInputLatches          .= Left Nothing
            executeStage.subPipeline .= []
            wrbInputLatches          .= Left Nothing
            invalidate               .= False
        unless :: Lens' Processor Bool -> ProcessorState () -> ProcessorState ()
        unless cond m = do b <- use cond
                           i <- use invalidate
                           if not b && not i then m
                           else return ()
        updateStallCounts :: ProcessorState ()
        updateStallCounts = do
          f <- use $ fetchStage.isStalled
          when f $ simData.fetchStalledCount     += 1
          d <- use $ decodeStage.isStalled
          when d $ simData.decodeStalledCount    += 1
          i <- use $ issueStage.isStalled
          when i $ simData.issueStalledCount     += 1
          e <- use $ executeStage.isStalled
          when e $ simData.executeStalledCount   += 1
          w <- use $ writebackStage.isStalled
          when w $ simData.writebackStalledCount += 1

superscalarProcessor :: ProcessorState ()
superscalarProcessor = undefined
