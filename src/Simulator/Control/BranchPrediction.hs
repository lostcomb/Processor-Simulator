module Simulator.Control.BranchPrediction
  ( predict
  , branch
  ) where

import Data.Bits
import Data.Word
import Data.Maybe
import Control.Lens

import Simulator.Data.Processor

predict :: Word32 -> (Word8, Word8, Word8, Word8) -> ProcessorState Control
predict pc b = do
  predictionScheme <- use $ options.branchPrediction
  case predictionScheme of
    Static     -> staticPredict     pc b
    Saturating -> saturatingPredict pc b
    TwoLevel   -> twoLevelPredict   pc b

staticPredict :: Word32 -> (Word8, Word8, Word8, Word8) -> ProcessorState Control
staticPredict pc (b1, b2, b3, b4)
  | op_code == 8 || op_code == 9 = return $ NotTaken pc
  | otherwise                    = return $ NA
  where op_code = (b1 .&. 0xF0) `shiftR` 4
        b3_32   = (fromIntegral b3) :: Word32
        b4_32   = (fromIntegral b4) :: Word32
        target  = fromIntegral $ (b3_32 `shiftL` 8) .|. b4_32

saturatingPredict :: Word32 -> (Word8, Word8, Word8, Word8) -> ProcessorState Control
saturatingPredict pc (b1, b2, b3, b4) = case op_code of
  -- JMP Case.
  8 -> do (ta, sat) <- use $ btac.entry pc
          if predictTaken sat && ta /= Nothing
            then return $ Taken    pc (fromJust ta)
            else return $ NotTaken pc
  -- BEZ Case.
  9 -> do (_, sat) <- use $ btac.entry pc
          if predictTaken sat
            then return $ Taken    pc target
            else return $ NotTaken pc
  _ -> return NA
  where op_code = (b1 .&. 0xF0) `shiftR` 4
        b3_32   = (fromIntegral b3) :: Word32
        b4_32   = (fromIntegral b4) :: Word32
        target  = fromIntegral $ (b3_32 `shiftL` 8) .|. b4_32

twoLevelPredict :: Word32 -> (Word8, Word8, Word8, Word8) -> ProcessorState Control
twoLevelPredict = undefined -- TODO

branch :: Bool -> Maybe Word32 -> Control -> ProcessorState ()
branch t ta c = do
  predictionScheme <- use $ options.branchPrediction
  case predictionScheme of
    Static     -> staticBranch     t ta c
    Saturating -> saturatingBranch t ta c
    TwoLevel   -> twoLevelBranch   t ta c

staticBranch :: Bool -> Maybe Word32 -> Control -> ProcessorState ()
staticBranch _ _ _ = return ()

saturatingBranch :: Bool -> Maybe Word32 -> Control -> ProcessorState ()
saturatingBranch taken ta c = do
  let pc = getPC c
  (tar, sat) <- use $ btac.entry pc
  case ta of
    Nothing -> btac.entry pc .= (tar   , nextState sat taken)
    Just a  -> btac.entry pc .= (Just a, nextState sat taken)

twoLevelBranch :: Bool -> Maybe Word32 -> Control -> ProcessorState ()
twoLevelBranch = undefined -- TODO
