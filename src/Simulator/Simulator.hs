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
  = do -- Set all latches to Nothing if the pipeline is to be invalidated.
       checkForInvalidation
       -- Get the value of the latches after the previous cycle.
       (Left d) <- use decInputLatches
       (Left i) <- use issInputLatches
       (Left e) <- use exeInputLatches
       (Left w) <- use wrbInputLatches
       -- Execute the Fetch stage unless it is stalled.
       unless (fetchStage.stalled) $ do
         f' <- pipelinedFetch
         decInputLatches .= Left f'
       -- Execute the Decode stage unless it is stalled.
       unless (decodeStage.stalled) $ do
         d' <- pipelinedDecode d
         issInputLatches .= Left d'
       -- Execute the Issue stage unless it is stalled.
       unless (issueStage.stalled) $ do
         i' <- pipelinedIssue i
         exeInputLatches .= Left i'
       -- Execute the Execute stage unless it is stalled.
       unless (executeStage.stalled) $ do
         e' <- pipelinedExecute e
         case e' of
           (Left    issuedData) -> do exeInputLatches .= Left issuedData
                                      wrbInputLatches .= Left Nothing
           (Right executedData) -> do wrbInputLatches .= Left executedData
       -- Execute the Writeback stage unless it is stalled.
       unless (writebackStage.stalled) $ do
         pipelinedWriteback w
       -- Increment the number of cycles executed.
       simData.cycles += 1
  where checkForInvalidation = do
          i <- use invalidate
          when i $ do
            decInputLatches .= Left Nothing
            issInputLatches .= Left Nothing
            exeInputLatches .= Left Nothing
            wrbInputLatches .= Left Nothing
            invalidate      .= False
        unless cond m = do b <- use cond
                           if not b then do m
                           else return ()

superscalarProcessor :: ProcessorState ()
superscalarProcessor = undefined
