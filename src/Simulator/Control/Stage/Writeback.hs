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
superscalarWriteback = mapM_ writeback

writeback :: ExecutedData -> ProcessorState ()
writeback (Nothing) = return ()
writeback (Just  d) = do simData.wrbInsts += 1
                         regFile.regVal pc += instLength
                         case d of
                           (inst_id, Just (r, v), inv) -> do
                             checkForHalt inst_id
                             regFile.regVal  r .= v
                             pt <- use $ options.procType
                             if pt /= Superscalar then do
                               regFile.regFlag r -= 1
                              else do
                                -- Update the register alias table if this
                                -- instruction is the last to modify a register.
                                s <- use $ registerAliasTable.status r
                                when (fromMaybe (-1) s == inst_id) $ do
                                  registerAliasTable.status r .= Nothing
                             checkForInvalidation inv $ fromIntegral v
                           (inst_id, Nothing    , inv) -> do
                             checkForHalt inst_id
                             pc_val <- use $ regFile.regVal pc
                             checkForInvalidation inv $ fromIntegral pc_val

checkForInvalidation :: Bool -> Word32 -> ProcessorState ()
checkForInvalidation inv v = when inv $ do
  fetchStage.programCounter .= v
  invalidate .= True
  cleanRegisters

checkForHalt :: Int -> ProcessorState ()
checkForHalt iid = do
  ha <- use $ writebackStage.haltAfter
  case ha of
    Just inst_id -> when (iid == inst_id) $ halted .= True
    Nothing      -> return ()

cleanRegisters :: ProcessorState ()
cleanRegisters = mapM_ (\r -> regFile.regFlag r .= clean) [(minBound::Register)..]
