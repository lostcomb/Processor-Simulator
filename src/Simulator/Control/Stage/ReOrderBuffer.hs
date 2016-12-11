{-# LANGUAGE FlexibleContexts #-}
module Simulator.Control.Stage.ReOrderBuffer
  ( superscalarReOrderBuffer
  ) where

import Data.Int
import Data.List
import Data.Maybe
import Control.Lens
import Control.Monad

import Debug.Trace

import Simulator.Data.Processor

superscalarReOrderBuffer :: ([ DecodedData ], [ ExecutedData ])
                         -> ProcessorState (Maybe [ DecodedData ], [ ExecutedData ])
superscalarReOrderBuffer (dd, ed)
  = do rob        <- use $ robStage.buffer
       let dd' = map fromJust . filter isJust $ dd
           ed' = map fromJust . filter isJust $ ed
           allocateable   = if branchInFlight rob && containsStore dd'
                              then takeWhile (\(Instruction _ i _) -> not . isStm $ i) dd'
                              else dd'
           unallocateable = map (\x -> Just x) . drop (length allocateable) $ dd'
       unallocateable' <- allocateInsts allocateable
       let stalled_i = if unallocateable' ++ unallocateable == []
                         then Nothing
                         else Just $ unallocateable' ++ unallocateable
       updateInsts ed'
       ci <- completedInsts

       if stalled_i /= Nothing then do
         fetchStage.stalled.byReOrderBuffer  .= True
         decodeStage.stalled.byReOrderBuffer .= True
       else do
         fetchStage.stalled.byReOrderBuffer  .= False
         decodeStage.stalled.byReOrderBuffer .= False

       return (stalled_i, ci)

branchInFlight :: [ ReOrderBufferEntry ] -> Bool
branchInFlight = foldl (\b (_, t, _, _, v, c) -> b || (t == Control && v && not c)) False

containsStore :: [ InstructionReg ] -> Bool
containsStore = any (\(Instruction _ i _) -> isStm i)

-- |This function updates the reorder buffer entries with the specified results.
updateInsts :: [ (Int, Maybe (Register, Int32), Bool) ] -> ProcessorState ()
updateInsts []                         = return ()
updateInsts ((inst_id, value, inv):is) = do
  rob <- use $ robStage.buffer
  let updateEntry (iid, t, r, v, valid, c) = if iid == inst_id
        then case value of
          Just (reg, val) -> (iid, t, reg, Just val, valid, True)
          Nothing         -> (iid, t,   r,        v, valid, True)
        else (iid, t, r, v, valid, c)
      rob' = map updateEntry rob
      invalidate (iid, t, r, v, _, c) = (iid, t, r, v, False, c)
      (Just index) = trace ("(" ++ show inst_id ++ ", " ++ show value ++ "," ++ show inv ++ ")") $ lookupIndex inst_id rob'
      before = take (index + 1) rob'
      after  = drop (index + 1) rob'
      after' = if inv then map invalidate after
                      else after
  -- If branch, update fetch stage pc and invalidate pipeline.
  checkForBranch value inv
  robStage.buffer .= (before ++ after')
  -- Update the contents of the reservation stations.
  rss <- use reservationStations
  let val = snd . fromMaybe (pc, 0) $ value
  reservationStations .= map (updatePointerValues (inst_id, val)) rss
  updateInsts is
  where checkForBranch (Nothing    ) _   = return ()
        checkForBranch (Just (r, v)) inv = do
          i <- use invalidate
          when (r == pc && inv && not i) $ do
            fetchStage.programCounter .= fromIntegral v
            invalidate .= True
            registerAliasTable .= newRegisterAliasTable

-- |This function returns a list of completed instructions in program order in
--  the rob.
completedInsts :: ProcessorState [ ExecutedData ]
completedInsts = do
  rob <- use $ robStage.buffer
  n   <- use $ options.noInstsPerCycle
  let completed  = take n . takeWhile (\(_, _, _, _, _, c) -> c) $ rob
      completed' = filter (\(_, _, _, _, v, _) -> v) completed
  robStage.buffer .= drop (length completed) rob
  return . map (\(inst_id, _, d, v, _, _) -> case v of
                    Just val -> Just (inst_id, Just (d, val), False)
                    Nothing  -> Just (inst_id, Nothing      , False)) $ completed'

-- |This function allocates the specified instructions in the reorder buffer and
--  distributes them to the reservation stations. It only returns the instructions
--  that can not be allocated.
allocateInsts :: [ InstructionReg ] -> ProcessorState [ DecodedData ]
allocateInsts []                        = return []
allocateInsts ((Instruction c i co):is) = do
  rob        <- use $ robStage.buffer
  rob_size   <- use $ options.robSize
  rss        <- use $ reservationStations
  shelf_size <- use $ options.shelfSize
  let cmp           = (\(_, (x, _, _)) (_, (y, _, _)) -> compare (length x) (length y))
      (index, (rs, b, n))   = if isLdm i || isStm i then (0, head rss)
                                            else minimumBy cmp . zip [1,2..] $ (tail rss)
      rob_entries   = rob_size - length rob
      shelf_entries = shelf_size - length rs
  if rob_entries > 0 && shelf_entries > 0 then do
    next_id <- use $ robStage.nextId
    robStage.nextId .= (next_id + 1) `mod` rob_size
    ((qi, vi), (qj, vj)) <- getOperandPointers next_id i
    let dest        = fromMaybe pc . instDestination $ i
        rob_entry   = (next_id, instType i, dest, Nothing, True, False)
        shelf_entry = (next_id, Instruction c i co, qi, qj, vi, vj)
    robStage.buffer .= (rob ++ [ rob_entry ])
    reservationStations %= update index (rs ++ [ shelf_entry ], b, n)

    allocateInsts is
  else do
    return . map (\x -> Just x) $ (Instruction c i co) : is

-- |This function returns the reservation station pointers and values for the
--  specified instruction.
getOperandPointers :: Int -> Inst Register -> ProcessorState ((Maybe Int, Int32), (Maybe Int, Int32))
getOperandPointers iid i = case i of
  (Nop         ) -> (,) <$>                  defValues    <*> defValues
  (Add rd ri rj) -> (,) <$  updateRAT rd <*> getValues ri <*> getValues rj
  (Sub rd ri rj) -> (,) <$  updateRAT rd <*> getValues ri <*> getValues rj
  (Mul rd ri rj) -> (,) <$  updateRAT rd <*> getValues ri <*> getValues rj
  (Div rd ri rj) -> (,) <$  updateRAT rd <*> getValues ri <*> getValues rj
  (And rd ri rj) -> (,) <$  updateRAT rd <*> getValues ri <*> getValues rj
  (Or  rd ri rj) -> (,) <$  updateRAT rd <*> getValues ri <*> getValues rj
  (Not rd ri   ) -> (,) <$  updateRAT rd <*> getValues ri <*> defValues
  (Jmp    ri   ) -> (,) <$>                  getValues ri <*> defValues
  (Bez    ri  o) -> (,) <$>                  getValues ri <*> defValues
  (Ceq rd ri rj) -> (,) <$  updateRAT rd <*> getValues ri <*> getValues rj
  (Cgt rd ri rj) -> (,) <$  updateRAT rd <*> getValues ri <*> getValues rj
  (Ldc rd     c) -> (,) <$  updateRAT rd <*> defValues    <*> defValues
  (Ldm rd ri   ) -> (,) <$  updateRAT rd <*> getValues ri <*> defValues
  (Stm    ri rj) -> (,) <$>                  getValues ri <*> getValues rj
  (Halt        ) -> (,) <$>                  defValues    <*> defValues
  where defValues   = return (Nothing, 0)
        getValues r = do rob <- use $ robStage.buffer
                         s   <- use $ registerAliasTable.status r
                         case s of
                           Just inst_id -> case lookupIndex inst_id rob of
                             Just index -> do
                               let (_, _, _, v, valid, c) = rob !! index
                               if valid && c then do return (Nothing, fromJust v)
                                             else do v <- use $ regFile.regVal r
                                                     return (Nothing, v)
                             Nothing    -> return (Just inst_id, 0)
                           Nothing      -> do v <- use $ regFile.regVal r
                                              return (Nothing, v)
        updateRAT r = registerAliasTable.status r .= Just iid

-- |This function updates the value at index @n@ with @val@.
update :: Int -> a -> [ a ] -> [ a ]
update n val xs = take n xs ++ val : drop (n + 1) xs

-- |This function returns the index of the reorder buffer entry with the
--  corresponding instruction id.
lookupIndex :: Int -> [ ReOrderBufferEntry ] -> Maybe Int
lookupIndex iid rob = lookupIndex' iid rob
  where lookupIndex' _ [] = Nothing
        lookupIndex' iid ((inst_id, _, _, _, _, _):is)
          | iid == inst_id = Just $ length rob - (length is + 1)
          | otherwise      = lookupIndex' iid is
