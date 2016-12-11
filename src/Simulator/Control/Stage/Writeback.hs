{-# LANGUAGE FlexibleContexts #-}
module Simulator.Control.Stage.Writeback
  ( scalarWriteback
  , pipelinedWriteback
  , superscalarWriteback
  ) where

import Data.Int
import Data.Word
import Data.Maybe
import Control.Lens
import Control.Monad
import Control.Monad.State

import Simulator.Data.Processor

scalarWriteback :: ExecutedData -> ProcessorState ()
scalarWriteback = writeback

pipelinedWriteback :: ExecutedData -> ProcessorState ()
pipelinedWriteback = writeback

superscalarWriteback :: [ ExecutedData ] -> ProcessorState ()
superscalarWriteback = mapM_ writeback'
  where writeback' (Nothing) = return ()
        writeback' (Just  d) = do simData.insts += 1
                                  regFile.regVal pc += instLength
                                  case d of
                                    (instId, Just (r, v), _) -> do
                                      regFile.regVal  r .= v
                                      -- Update the register alias table if this
                                      -- instruction is the last to modify a register.
                                      s <- use $ registerAliasTable.status r
                                      when (fromMaybe (-1) s == instId) $ do
                                        registerAliasTable.status r .= Nothing
                                    (_     , Nothing    , _) -> return ()

writeback :: ExecutedData -> ProcessorState ()
writeback (Nothing) = return ()
writeback (Just  d) = do simData.insts += 1
                         regFile.regVal pc += instLength
                         case d of
                           (_, Just (r, v), inv) -> do
                             regFile.regVal  r .= v
                             regFile.regFlag r -= 1
                             checkForInvalidation inv $ fromIntegral v
                           (_, Nothing    , inv) -> do
                             pc_val <- use $ regFile.regVal pc
                             checkForInvalidation inv $ fromIntegral pc_val

checkForInvalidation :: Bool -> Word32 -> ProcessorState ()
checkForInvalidation inv v = when inv $ do
  fetchStage.programCounter .= v
  invalidate .= True
  cleanRegisters

cleanRegisters :: ProcessorState ()
cleanRegisters = mapM_ (\r -> regFile.regFlag r .= clean) [(minBound::Register)..]
