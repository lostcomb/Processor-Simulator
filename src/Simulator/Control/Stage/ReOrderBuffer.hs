{-# LANGUAGE FlexibleContexts #-}
module Simulator.Control.Stage.ReOrderBuffer
  ( superscalarReOrderBuffer
  ) where

import Data.Int
import Data.Maybe
import Control.Lens
import Control.Monad

import Simulator.Data.Processor

superscalarReOrderBuffer :: ([ DecodedData ], [ ExecutedData ])
                         -> ProcessorState (Maybe [ DecodedData ], [ ExecutedData ])
superscalarReOrderBuffer (dd, ed)
  = do --Perform stalling logic here, so functions don't need to worry about it.
       ed'        <- updateInsts ed
       (mdd, dd') <- allocateInsts dd
       distributeInsts dd'
       return (if mdd == [] then Nothing else Just mdd, ed')
           --TODO: Get the updated rob after putting the executed data values into it.
           --      Also, get the values to be written to the registers, maximum
           --      of no_insts_per_cycle.
           --      If inv flag is true for any incoming data item, set the valid
           --      flag of all after it in the rob to false, and update the pc val
           --      in the fetchStage.
           --      If the valid flag is false, don't write to registers.
           --      If the rob is full, stall previous stages.
           --      Add the decoded instructions to the correct reservation stations
           --      (rs0 is for laod/stores only and must operate in-order at all times).
           --      If all of the reservation stations are full, stall previous stages.

-- |This function puts the values of instructions into their respective places
--  in the reorder buffer. It then returns the list of instructions that have
--  completed in program order.
updateInsts :: [ ExecutedData ] -> ProcessorState [ ExecutedData ]
updateInsts ed = do mapM_ updateInst ed
                    getCompleted
  where updateInst (Nothing               ) = return ()
        updateInst (Just (instId, rv, inv)) = do
          rob       <- use $ robStage.buffer
          (rob', _) <- foldM (\(rob', valid') (iId, t, reg, val, valid, c) -> do
                            if iId == instId then do
                              let value = case rv of
                                            Just (_, v') -> Just v'
                                            Nothing      -> Nothing
                                  inst = (iId, t, reg, value, valid', True)
                              return (rob' ++ [inst], not inv)
                            else do
                              let inst = (iId, t, reg, val, valid', c)
                              return (rob' ++ [inst], valid')) ([], False) rob
          robStage.buffer .= rob'
        getCompleted = do
          rob <- use $ robStage.buffer
          n <- use $ options.noInstsPerCycle
          let wb   = takeWhile (\(_, _, _, _, _, completed) -> completed) . take n $ rob
              rob' = drop (length wb) rob
              wb'  = map (\(instId, _, r, v, _, _) -> case v of
                             (Just val) -> Just (instId, Just (r, val), False)
                             (Nothing ) -> Just (instId, Nothing      , False))
                   . filter (\(_, _, _, _, valid, _) -> valid) $ wb
          robStage.buffer .= rob'
          return wb'

-- |This function converts the decoded instructions into reorder buffer entries.
allocateInsts :: [ DecodedData ] -> ProcessorState ([ DecodedData ], [ (Int, DecodedData) ])
allocateInsts = foldM allocateInst ([], []) . filter isJust
  where allocateInst (dd, rb) (Just (Instruction c i co)) = do
          rob <- use $ robStage.buffer
          rob_size <- use $ options.robSize
          if length rob < rob_size then do
            --if isStm i && any (\) rob -- TODO Take into account Stores after branches.
            if (not (isNop i || isStm i || isHalt i)) then do
              nId <- use $ robStage.nextId
              let entry = (nId, instType i, fromMaybe pc . instDestination $ i, Nothing, True, False)
                  nId'  = (nId + 1) `mod` rob_size
              robStage.nextId .= nId'
              robStage.buffer .= (rob ++ [ entry ])
              return (dd, rb ++ [ (nId, Just (Instruction c i co)) ])
            else do
              return (dd, rb ++ [ ((-1), Just (Instruction c i co)) ])
          else do
            return (dd ++ [ Just (Instruction c i co) ], rb)


-- |This function distributes the decoded instructions evenly among the
--  reservation stations.
distributeInsts :: [ (Int, DecodedData) ] -> ProcessorState ()
distributeInsts = mapM_ distributeInst
  where distributeInst (instId, dd) = undefined

-- |This function returns the reservation station pointers and values for the
--  specified instruction.
getOperandPointers :: InstructionReg -> ProcessorState (Maybe Int, Maybe Int, Int32, Int32)
getOperandPointers (Instruction _ i _) = case i of
  (Nop         ) -> (,,,) <$> defStatus    <*> defStatus    <*> defValue    <*> defValue
  (Add rd ri rj) -> (,,,) <$> getStatus ri <*> getStatus rj <*> getValue ri <*> getValue rj
  (Sub rd ri rj) -> (,,,) <$> getStatus ri <*> getStatus rj <*> getValue ri <*> getValue rj
  (Mul rd ri rj) -> (,,,) <$> getStatus ri <*> getStatus rj <*> getValue ri <*> getValue rj
  (Div rd ri rj) -> (,,,) <$> getStatus ri <*> getStatus rj <*> getValue ri <*> getValue rj
  (And rd ri rj) -> (,,,) <$> getStatus ri <*> getStatus rj <*> getValue ri <*> getValue rj
  (Or  rd ri rj) -> (,,,) <$> getStatus ri <*> getStatus rj <*> getValue ri <*> getValue rj
  (Not rd ri   ) -> (,,,) <$> getStatus ri <*> defStatus    <*> getValue ri <*> defValue
  (Jmp    ri   ) -> (,,,) <$> getStatus ri <*> defStatus    <*> getValue ri <*> defValue
  (Bez    ri  o) -> (,,,) <$> getStatus ri <*> defStatus    <*> getValue ri <*> defValue
  (Ceq rd ri rj) -> (,,,) <$> getStatus ri <*> getStatus rj <*> getValue ri <*> getValue rj
  (Cgt rd ri rj) -> (,,,) <$> getStatus ri <*> getStatus rj <*> getValue ri <*> getValue rj
  (Ldc rd     c) -> (,,,) <$> defStatus    <*> defStatus    <*> defValue    <*> defValue
  (Ldm rd ri   ) -> (,,,) <$> getStatus ri <*> defStatus    <*> getValue ri <*> defValue
  (Stm    ri rj) -> (,,,) <$> getStatus ri <*> getStatus rj <*> getValue ri <*> getValue rj
  (Halt        ) -> (,,,) <$> defStatus    <*> defStatus    <*> defValue    <*> defValue
  where defValue  = return 0
        defStatus = return Nothing
        getStatus r = use $ registerAliasTable.status r
        getValue  r = do s <- getStatus r
                         if s == Nothing then use $ regFile.regVal r
                                         else defValue -- Look in the rob first.
