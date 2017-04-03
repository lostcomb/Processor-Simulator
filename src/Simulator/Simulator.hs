{-
This file is part of aca-processor-simulator.

aca-processor-simulator is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

aca-processor-simulator is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with aca-processor-simulator.  If not, see <http://www.gnu.org/licenses/>.
-}
{-# LANGUAGE RankNTypes #-}
module Simulator.Simulator
  ( runProcessor
  ) where

import System.IO
import Data.Maybe
import Control.Lens
import Control.Monad (forever)
import Control.Monad.State

import Simulator.Data.Processor
import Simulator.Control.Stage.Fetch
import Simulator.Control.Stage.Decode
import Simulator.Control.Stage.ReOrderBuffer
import Simulator.Control.Stage.Issue
import Simulator.Control.Stage.Execute
import Simulator.Control.Stage.Writeback
import Simulator.CommandLine.Parser
import Simulator.CommandLine.Command
import Simulator.CommandLine.Interpreter

copyrightNotice :: String
copyrightNotice =  "Processor Simulator  Copyright (C) 2016  Julian Loscombe\n"
                ++ "This program comes with ABSOLUTELY NO WARRANTY; for details type `show w'.\n"
                ++ "This is free software, and you are welcome to redistribute it\n"
                ++ "under certain conditions; type `show c' for details.\n"

runProcessor :: StateT [ Command ] (StateT Processor IO) a
runProcessor = do liftIO $ putStrLn copyrightNotice
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
            fetchStage.stalled        .= newStalled
            decInputLatches           .= Left Nothing
            decodeStage.stalled       .= newStalled
            issInputLatches           .= Left Nothing
            issueStage.stalled        .= newStalled
            exeInputLatches           .= Left Nothing
            executeStage.stalled      .= newStalled
            executeStage.subPipeline  .= newSubPipeline 1
            executeStage.bypassValues .= Nothing
            wrbInputLatches           .= Left Nothing
            writebackStage.stalled    .= newStalled
            writebackStage.haltAfter  .= Nothing
            fetchStage.halt           .= False
            invalidate                .= False
        unless :: Lens' Processor Bool -> ProcessorState () -> ProcessorState ()
        unless cond m = do b <- use cond
                           i <- use invalidate
                           when (not b && not i) m

superscalarProcessor :: ProcessorState ()
superscalarProcessor
  = do -- Update the stall counts.
       updateStallCounts
       -- Get the value of the latches after the previous cycle.
       (Right d) <- use decInputLatches
       (dd, ed)  <- use robInputLatches
       (Right e) <- use exeInputLatches
       (Right w) <- use wrbInputLatches
       -- Execute the Writeback stage unless it is stalled.
       unless (writebackStage.isStalled) $ do
         superscalarWriteback w
       -- Execute the Execute stage unless it is stalled.
       unless (executeStage.isStalled) $ do
         e' <- superscalarExecute e
         let (i', ed') = unzip . map (\exec -> case exec of
                                       Left    issuedData -> (issuedData, Nothing     )
                                       Right executedData -> (Nothing   , executedData)) $ e'
         (dd'', _) <- use robInputLatches
         robInputLatches .= (dd'', ed')
         exeInputLatches .= Right i'
       -- Execute the Issue stage unless it is stalled.
       unless (issueStage.isStalled) $ do
         i' <- superscalarIssue
         (Right i'') <- use exeInputLatches
         let merge (Nothing, x) = x
             merge (y      , _) = y
             i'''  = map merge . zip i' $ i''
         exeInputLatches .= Right i'''
       -- Execute the ReOrderBuffer stage unless it is stalled.
       unless (robStage.isStalled) $ do
         (dd', ed') <- superscalarReOrderBuffer (dd, ed)
         wrbInputLatches .= Right ed'
         when (dd' /= Nothing) $ do
           (_, ed'') <- use robInputLatches
           robInputLatches .= (fromJust dd', ed'')
       -- Execute the Decode stage unless it is stalled.
       unlessi (decodeStage.isStalled) $ do
         d' <- superscalarDecode d
         (_, ed') <- use robInputLatches
         robInputLatches .= (d', ed')
       -- Execute the Fetch stage unless it is stalled.
       unlessi (fetchStage.isStalled) $ do
         f' <- superscalarFetch
         decInputLatches .= Right f'
       -- Set decode and reorder buffer latches to Nothing if the pipeline is
       -- to be invalidated.
       checkForInvalidation
       -- Increment the number of cycles executed.
       simData.cycles += 1
  where checkForInvalidation :: ProcessorState ()
        checkForInvalidation = do
          i      <- use invalidate
          n      <- use $ options.noInstsPerCycle
          no_eus <- use $ options.noEUs
          when i $ do
            -- Clear the latches.
            decInputLatches .= Right (replicate n      Nothing)
            robInputLatches .= ([], [])
            exeInputLatches .= Right (replicate no_eus Nothing)
            executeStage.subPipeline .= newSubPipeline no_eus
            executeStage.bypassValues .= Nothing
            wrbInputLatches .= Right (replicate n      Nothing)
            -- Clear the stalled flags.
            fetchStage.stalled     .= newStalled
            decodeStage.stalled    .= newStalled
            robStage.stalled       .= newStalled
            issueStage.stalled     .= newStalled
            executeStage.stalled   .= newStalled
            writebackStage.stalled .= newStalled
            -- Reset the invalidate / halt flags.
            invalidate               .= False
            fetchStage.halt          .= False
            writebackStage.haltAfter .= Nothing
            -- Empty the reservation stations.
            shelf_size <- use $ options.shelfSize
            let rs = newReservationStation shelf_size
            reservationStations .= replicate no_eus rs
            -- Empty the reorder buffer.
            robStage.buffer .= []
            -- Empty the register alias table.
            registerAliasTable .= newRegisterAliasTable
        unless  :: Lens' Processor Bool -> ProcessorState () -> ProcessorState ()
        unless  cond m = do b <- use cond
                            when (not b) m
        unlessi :: Lens' Processor Bool -> ProcessorState () -> ProcessorState ()
        unlessi cond m = do b <- use cond
                            i <- use invalidate
                            when (not b && not i) m

updateStallCounts :: ProcessorState ()
updateStallCounts = do
  f <- use $ fetchStage.isStalled
  when f $ simData.fetchStalledCount     += 1
  d <- use $ decodeStage.isStalled
  when d $ simData.decodeStalledCount    += 1
  r <- use $ robStage.isStalled
  when r $ simData.robStalledCount       += 1
  i <- use $ issueStage.isStalled
  when i $ simData.issueStalledCount     += 1
  e <- use $ executeStage.isStalled
  when e $ simData.executeStalledCount   += 1
  w <- use $ writebackStage.isStalled
  when w $ simData.writebackStalledCount += 1
