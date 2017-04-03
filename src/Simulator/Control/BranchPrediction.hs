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
module Simulator.Control.BranchPrediction
  ( predict
  , branch
  ) where

import Data.Bits
import Data.Word
import Data.Maybe
import Control.Lens

import Simulator.Data.Processor

-- |This function determines which branch prediction scheme to use. It returns
--  a control data type which determines whether the branch is taken or not
--  taken.
predict :: Word32 -> (Word8, Word8, Word8, Word8) -> ProcessorState Control
predict pc b = do
  predictionScheme <- use $ options.branchPrediction
  case predictionScheme of
    Static     -> staticPredict     pc b
    Saturating -> saturatingPredict pc b
    TwoLevel   -> twoLevelPredict   pc b

-- |This function uses a static always not taken branch prediction scheme. It
--  returns a control data type which determines whether the branch is taken or
--  not taken.
staticPredict :: Word32 -> (Word8, Word8, Word8, Word8) -> ProcessorState Control
staticPredict pc (b1, b2, b3, b4)
  | op_code == 8 || op_code == 9 = return $ NotTaken pc
  | otherwise                    = return $ NA       pc
  where op_code = (b1 .&. 0xF0) `shiftR` 4
        b3_32   = (fromIntegral b3) :: Word32
        b4_32   = (fromIntegral b4) :: Word32
        target  = fromIntegral $ (b3_32 `shiftL` 8) .|. b4_32

-- |This function uses a saturating counter branch prediction scheme. It
--  returns a control data type which determines whether the branch is taken or
--  not taken.
saturatingPredict :: Word32 -> (Word8, Word8, Word8, Word8) -> ProcessorState Control
saturatingPredict pc (b1, b2, b3, b4) = case op_code of
  -- JMP Case.
  8 -> do (ta, Left sat) <- use $ btac.saturationEntry pc
          if predictTaken sat && ta /= Nothing
            then return $ Taken    pc (fromJust ta)
            else return $ NotTaken pc
  -- BEZ Case.
  9 -> do (_, Left sat) <- use $ btac.saturationEntry pc
          if predictTaken sat
            then return $ Taken    pc target
            else return $ NotTaken pc
  _ -> return $ NA pc
  where op_code = (b1 .&. 0xF0) `shiftR` 4
        b3_32   = (fromIntegral b3) :: Word32
        b4_32   = (fromIntegral b4) :: Word32
        target  = fromIntegral $ (b3_32 `shiftL` 8) .|. b4_32

-- |This function uses a two-level adaptive branch prediction scheme. It
--  returns a control data type which determines whether the branch is taken or
--  not taken. Documented here: http://www.seas.upenn.edu/~cis501/papers/two-level-branch-pred.pdf
twoLevelPredict :: Word32 -> (Word8, Word8, Word8, Word8) -> ProcessorState Control
twoLevelPredict pc (b1, b2, b3, b4) = case op_code of
  -- JMP Case.
  8 -> do (ta, Right hist) <- use $ btac.twoLevelEntry pc
          sat              <- use $ patternHistory.entry hist
          if predictTaken sat && ta /= Nothing
            then return $ Taken    pc (fromJust ta)
            else return $ NotTaken pc
  -- BEZ Case.
  9 -> do (_, Right hist) <- use $ btac.twoLevelEntry pc
          sat             <- use $ patternHistory.entry hist
          if predictTaken sat
            then return $ Taken    pc target
            else return $ NotTaken pc
  _ -> return $ NA pc
  where op_code = (b1 .&. 0xF0) `shiftR` 4
        b3_32   = (fromIntegral b3) :: Word32
        b4_32   = (fromIntegral b4) :: Word32
        target  = fromIntegral $ (b3_32 `shiftL` 8) .|. b4_32

-- |This function updates the BTAC after a branch has been taken.
branch :: Bool -> Maybe Word32 -> Control -> ProcessorState ()
branch t ta c = do
  predictionScheme <- use $ options.branchPrediction
  case predictionScheme of
    Static     -> staticBranch     t ta c
    Saturating -> saturatingBranch t ta c
    TwoLevel   -> twoLevelBranch   t ta c

-- |This function does nothing as the static branch prediction scheme doesn't
--  use any history.
staticBranch :: Bool -> Maybe Word32 -> Control -> ProcessorState ()
staticBranch _ _ _ = return ()

-- |This function updates the BTAC after a branch has been taken when using the
--  saturating counter branch prediction scheme.
saturatingBranch :: Bool -> Maybe Word32 -> Control -> ProcessorState ()
saturatingBranch taken ta c = do
  let pc = getPC c
  (tar, Left sat) <- use $ btac.saturationEntry pc
  case ta of
    Nothing -> btac.saturationEntry pc .= (tar   , Left $ nextState sat taken)
    Just a  -> btac.saturationEntry pc .= (Just a, Left $ nextState sat taken)

-- |This function updates the BTAC after a branch has been taken when using the
--  two level adaptive branch prediction scheme.
twoLevelBranch :: Bool -> Maybe Word32 -> Control -> ProcessorState ()
twoLevelBranch taken ta c = do
  let pc = getPC c
  (tar, Right hist) <- use $ btac.twoLevelEntry pc
  sat               <- use $ patternHistory.entry hist
  hist'             <- updateHist taken hist
  let sat' = nextState sat taken
  patternHistory.entry hist .= sat'
  case ta of
    Nothing -> btac.twoLevelEntry pc .= (tar   , Right hist')
    Just a  -> btac.twoLevelEntry pc .= (Just a, Right hist')

-- |This function updates the specified history using the taken boolean.
updateHist :: Bool -> Word32 -> ProcessorState Word32
updateHist taken hist = do
  k <- use $ options.branchHistoryBits
  let lsb   = if taken then 1 else 0
      hist' = ((hist `shiftL` 1) .|. lsb) .&. (0xFFFFFFFF `shiftR` fromIntegral (32 - k))
  return hist'
