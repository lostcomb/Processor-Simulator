module Simulator.Control.BranchPrediction
  ( predict
  , branch
  ) where

import Data.Bits
import Data.Word
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
staticPredict pc (b1, b2, b3, b4) = case (b1 .&. 0xF0) `shiftR` 4 of
  8 -> return $ NotTaken pc (pc + instLength)
  9 -> return $ NotTaken pc (pc + instLength)
  _ -> return $ NA
  where b3_32 = (fromIntegral b3) :: Word32
        b4_32 = (fromIntegral b4) :: Word32
        target = fromIntegral $ (b3_32 `shiftL` 8) .|. b4_32

saturatingPredict :: Word32 -> (Word8, Word8, Word8, Word8) -> ProcessorState Control
saturatingPredict = undefined -- TODO

twoLevelPredict :: Word32 -> (Word8, Word8, Word8, Word8) -> ProcessorState Control
twoLevelPredict = undefined -- TODO

branch :: Bool -> Control -> ProcessorState ()
branch t c = do
  predictionScheme <- use $ options.branchPrediction
  case predictionScheme of
    Static     -> staticBranch     t c
    Saturating -> saturatingBranch t c
    TwoLevel   -> twoLevelBranch   t c

staticBranch :: Bool -> Control -> ProcessorState ()
staticBranch taken c = return ()

saturatingBranch :: Bool -> Control -> ProcessorState ()
saturatingBranch = undefined -- TODO

twoLevelBranch :: Bool -> Control -> ProcessorState ()
twoLevelBranch = undefined -- TODO
