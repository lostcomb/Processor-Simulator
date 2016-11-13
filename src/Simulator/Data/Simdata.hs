module Simulator.Data.Simdata
  ( Simdata
  , newSimdata
  , cycles
  , insts
  , instsPerCycle
  , fetchStalledCount
  , decodeStalledCount
  , issueStalledCount
  , executeStalledCount
  , writebackStalledCount
  , correctPredictions
  , incorrectPredictions
  , outOfOrderPerCycle
  ) where

import Control.Lens
import Simulator.Data.Instruction

data Simdata = Simdata
  -- Counts the number of cycles executed.
  { _cycles                :: Int
  -- Counts the number of instrctions executed.
  , _insts                 :: Int
  -- Counts the number of instructions executed in each cycle during execution.
  , _instsPerCycle         :: [ Int ]
  -- Counts the number of cycles each stage is stalled during execution.
  , _fetchStalledCount     :: Int
  , _decodeStalledCount    :: Int
  , _issueStalledCount     :: Int
  , _executeStalledCount   :: Int
  , _writebackStalledCount :: Int
  -- Counts the number of correct and incorrect branch predictions.
  , _correctPredictions    :: [ InstructionReg ]
  , _incorrectPredictions  :: [ InstructionReg ]
  -- Counts the number of instructions executed out of order in each cycle
  -- during execution.
  , _outOfOrderPerCycle    :: [ Int ]
  }

newSimdata :: Simdata
newSimdata = Simdata
  { _cycles                = 0
  , _insts                 = 0
  , _instsPerCycle         = []
  , _fetchStalledCount     = 0
  , _decodeStalledCount    = 0
  , _issueStalledCount     = 0
  , _executeStalledCount   = 0
  , _writebackStalledCount = 0
  , _correctPredictions    = []
  , _incorrectPredictions  = []
  , _outOfOrderPerCycle    = []
  }

cycles :: Lens' Simdata Int
cycles = lens _cycles (\sim c -> sim { _cycles = c })

insts :: Lens' Simdata Int
insts = lens _insts (\sim i -> sim { _insts = i })

instsPerCycle :: Lens' Simdata [ Int ]
instsPerCycle = lens _instsPerCycle (\sim i -> sim { _instsPerCycle = i })

fetchStalledCount :: Lens' Simdata Int
fetchStalledCount = lens _fetchStalledCount (\sim f -> sim { _fetchStalledCount = f })

decodeStalledCount :: Lens' Simdata Int
decodeStalledCount = lens _decodeStalledCount (\sim d -> sim { _decodeStalledCount = d })

issueStalledCount :: Lens' Simdata Int
issueStalledCount = lens _issueStalledCount (\sim i -> sim { _issueStalledCount = i })

executeStalledCount :: Lens' Simdata Int
executeStalledCount = lens _executeStalledCount (\sim e -> sim { _executeStalledCount = e })

writebackStalledCount :: Lens' Simdata Int
writebackStalledCount = lens _writebackStalledCount (\sim w -> sim { _writebackStalledCount = w })

correctPredictions :: Lens' Simdata [ InstructionReg ]
correctPredictions = lens _correctPredictions (\sim c -> sim { _correctPredictions = c })

incorrectPredictions :: Lens' Simdata [ InstructionReg ]
incorrectPredictions = lens _incorrectPredictions (\sim i -> sim { _incorrectPredictions = i })

outOfOrderPerCycle :: Lens' Simdata [ Int ]
outOfOrderPerCycle = lens _outOfOrderPerCycle (\sim o -> sim { _outOfOrderPerCycle = o })
