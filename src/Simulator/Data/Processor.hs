{-# LANGUAGE TemplateHaskell,
             MultiParamTypeClasses,
             FunctionalDependencies,
             FlexibleInstances,
             RankNTypes #-}
module Simulator.Data.Processor
  ( module Simulator.Data.Processor
  , module Simulator.Data.Simdata
  , module Simulator.Data.Registers
  , module Simulator.Data.Instruction
  , module Simulator.Data.Stage.Fetch
  , module Simulator.Data.Stage.Decode
  , module Simulator.Data.Stage.Issue
  , module Simulator.Data.Stage.Execute
  , module Simulator.Data.Stage.Writeback
  ) where

import Data.Int
import Data.Word
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Control.Lens

import Simulator.Data.Simdata
import Simulator.Data.Registers
import Simulator.Data.Instruction
import Simulator.Data.Stage.Fetch
import Simulator.Data.Stage.Decode
import Simulator.Data.Stage.Issue
import Simulator.Data.Stage.Execute
import Simulator.Data.Stage.Writeback

-- Define types for memories.
type InstMem = Seq Word8
type DataMem = Map Word32 Word8

-- Let Template Haskell make the lenses for the stages.
makeFields ''Fetch
makeFields ''Decode
makeFields ''Issue
makeFields ''Execute
makeFields ''Writeback

-- Let Template Haskell make the lenses for Simdata.
makeFields ''Simdata

data Type = Scalar
          | Pipeline
          | Superscalar
          deriving (Show, Eq, Read)

data Options = Options
  { _procType      :: Type
  , _bypassEnabled :: Bool
  , _pipelinedEUs  :: Bool
  }
-- Let Template Haskell make the lenses.
makeFields ''Options

newOptions :: Options
newOptions = Options
  { _procType      = Scalar
  , _bypassEnabled = True
  , _pipelinedEUs  = True
  }

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
  , _instMem         :: InstMem
  , _dataMem         :: DataMem
  , _regFile         :: RegisterFile
  , _simData         :: Simdata
  , _instCycles      :: Inst Register -> Int
  , _options         :: Options
  }
-- Let Template Haskell make the lenses.
makeFields ''Processor

newProcessor :: [ Word8 ] -> Int -> Int -> Processor
newProcessor insts n_fetch n_eus = Processor
  { _fetchStage      = newFetch n_fetch
  , _decInputLatches = []
  , _decodeStage     = newDecode
  , _issInputLatches = []
  , _issueStage      = newIssue
  , _exeInputLatches = []
  , _executeStage    = newExecute n_eus
  , _wrbInputLatches = []
  , _writebackStage  = newWriteback
  , _instMem         = Seq.fromList insts
  , _dataMem         = Map.empty
  , _regFile         = newRegFile
  , _simData         = newSimdata
  , _instCycles      = const 1 --TODO
  , _options         = newOptions
  }

-- Define getters and setters for memory.
instMemItem :: Word32 -> Lens' InstMem Word8
instMemItem i = lens (\mem   -> Seq.index mem (fromIntegral i))
                     (\mem w -> Seq.update (fromIntegral i) w mem)

dataMemItem :: Word32 -> Lens' DataMem Word8
dataMemItem i = lens (\mem   -> Map.findWithDefault 0 i mem)
                     (\mem w -> Map.insert i w mem)
