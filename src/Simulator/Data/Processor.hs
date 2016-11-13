module Data.Processor
  (-- Processor types.
    Processor
   -- Processor functions
  , fetchStage, decInputLatches, decodeStage, issInputLatches, issueStage
  , exeInputLatches, executeStage, wrbInputLatches, writebackStage
  , instMem, dataMem, regFile, simData, instCycles, options
   -- Option types.
  , Options, Type(..)
   -- Option functions.
  , procType, bypassEnabled, pipelinedEUs
   -- Fetch type.
  , Fetch
   -- Fetch functions.
  , newFetch, noOfInsts, programCounter
   -- Decode type.
  , Decode
   -- Decode functions.
  , newDecode
   -- Issue type.
  , Issue
   -- Issue functions.
  , newIssue, issueWindow, execWindow
   -- Execute type.
  , Execute
   -- Execute functions.
  , newExecute, noOfEUs, bypassValues
   -- Writeback type.
  , Writeback
   -- Writeback functions.
  , newWriteback
   -- RegisterFile types.
  , Register(..), RegisterFile, Flag(..)
   -- RegisterFile functions.
  , pc, newRegFile
   -- Simdata type.
  , Simdata
   -- Simdata functions.
  , newSimdata, cycles, insts, instsPerCycle, fetchStalledCount
  , decodeStalledCount, issueStalledCount, executeStalledCount
  , writebackStalledCount, correctPredictions, incorrectPredictions
  , outOfOrderPerCycle
   -- Stallable typeclass.
  , Stallable(..)
  ) where

import Data.Int
import Data.Word
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

import Data.Simdata
import Data.Registers
import Data.Stallable
import Data.Instruction
import Data.Stage.Fetch
import Data.Stage.Decode
import Data.Stage.Issue
import Data.Stage.Execute
import Data.Stage.Writeback

-- |This data type contains all of the processors state.
data Processor = Processor
  { _fetchStage      :: Fetch
  , _decInputLatches :: [ Maybe (Word8, Word8, Word8, Word8) ]
  , _decodeStage     :: Decode
  , _issInputLatches :: [ Maybe InstructionReg ]
  , _issueStage      :: Issue
  , _exeInputLatches :: [ Maybe InstructionVal ]
  , _executeStage    :: Execute
  , _wrbInputLatches :: [ Maybe (Register, Int32) ]
  , _writebackStage  :: Writeback
  , _instMem         :: [ Word8 ]
  , _dataMem         :: Map Word32 Word8
  , _regFile         :: RegisterFile
  , _simData         :: Simdata
  , _instCycles      :: Inst Register -> Int
  , _options         :: Options
  }

fetchStage :: Lens' Processor Fetch
fetchStage = lens _fetchStage (\proc f -> proc { _fetchStage = f })

decInputLatches :: Lens' Processor [ Maybe (Word8, Word8, Word8, Word8) ]
decInputLatches = lens _decInputLatches (\proc d -> proc { _decInputLatches = d })

decodeStage :: Lens' Processor Decode
decodeStage = lens _decodeStage (\proc d -> proc { _decodeStage = d })

issInputLatches :: Lens' Processor [ Maybe InstructionReg ]
issInputLatches = lens _issInputLatches (\proc i -> proc { _issInputLatches = i })

issueStage :: Lens' Processor Issue
issueStage = lens _issueStage (\proc i -> proc { _issueStage = i })

exeInputLatches :: Lens' Processor [ Maybe InstructionVal ]
exeInputLatches = lens _exeInputLatches (\proc e -> proc { _exeInputLatches = e })

executeStage :: Lens' Processor Execute
executeStage = lens _executeStage (\proc e -> proc { _executeStage = e })

wrbInputLatches :: Lens' Processor [ Maybe (Register, Int32) ]
wrbInputLatches = lens _wrbInputLatches (\proc w -> proc { _wrbInputLatches = w })

writebackStage :: Lens' Processor Writeback
writebackStage = lens _writebackStage (\proc w -> proc { _writebackStage = w })

instMem :: Lens' Processor [ Word8 ]
instMem = lens _instMem (\proc i -> proc { _instMem = i })

dataMem :: Lens' Processor (Map Word32 Word8)
dataMem = lens _dataMem (\proc d -> proc { _dataMem = d })

regFile :: Lens' Processor RegisterFile
regFile = lens _regFile (\proc r -> proc { _regFile = r })

simData :: Lens' Processor Simdata
simData = lens _simData (\proc s -> proc { _simData = s })

instCycles :: Lens' Processor (Inst Register -> Int)
instCycles = lens _instCycles (\proc i -> proc { _instCycles = i })

options :: Lens' Processor Options
options = lens _options (\proc o -> proc { _options = o })

data Options = Options
  { _procType      :: Type
  , _bypassEnabled :: Bool
  , _pipelinedEUs  :: Bool
  }

procType :: Lens' Options Type
procType = lens _procType (\opts t -> opts { _procType = t })

bypassEnabled :: Lens' Options Bool
bypassEnabled = lens _bypassEnabled (\opts b -> opts { _bypassEnabled = b })

pipelinedEUs :: Lens' Options Bool
pipelinedEUs = lens _pipelinedEUs (\opts p -> opts { _pipelinedEUs = p })

data Type = Scalar
          | Pipeline
          | Superscalar
          deriving (Show, Eq, Read)
