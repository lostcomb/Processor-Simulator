{-# LANGUAGE TemplateHaskell,
             MultiParamTypeClasses,
             FunctionalDependencies,
             FlexibleInstances,
             RankNTypes #-}
module Simulator.Data.Processor
  ( module Simulator.Data.Processor
  , module Simulator.Data.Stall
  , module Simulator.Data.Stage
  , module Simulator.Data.Simdata
  , module Simulator.Data.Registers
  , module Simulator.Data.Instruction
  , module Simulator.Data.BTAC
  ) where

import Data.Int
import Data.Word
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Sequence (Seq)
import qualified Data.Sequence as Seq
import Control.Lens
import Control.Monad.State

import Simulator.Data.Stall
import Simulator.Data.Stage
import Simulator.Data.Simdata
import Simulator.Data.Registers
import Simulator.Data.Instruction
import Simulator.Data.BTAC

data Type = Scalar
          | Pipelined
          | Superscalar
          deriving (Show, Eq, Read)

data BranchPrediction = Static
                      | Saturating
                      | TwoLevel
                      deriving (Show, Eq, Read)

data Options = Options
  { _procType          :: Type
  , _bypassEnabled     :: Bool
  , _pipelinedEUs      :: Bool
  , _branchPrediction  :: BranchPrediction
  , _branchHistoryBits :: Word32
  , _noEUs             :: Int
  , _noInstsPerCycle   :: Int
  , _outOfOrder        :: Bool
  , help               :: Bool
  }
-- Let Template Haskell make the lenses.
makeLenses ''Options

defaultOptions :: Options
defaultOptions = Options
  { _procType          = Scalar
  , _bypassEnabled     = False
  , _pipelinedEUs      = False
  , _branchPrediction  = Static
  , _branchHistoryBits = 4
  , _noEUs             = 1
  , _noInstsPerCycle   = 1
  , _outOfOrder        = False
  , help               = False
  }

-- |These types correspond to the types of the output data for each stage.
type FetchedData  = Maybe (Word8, Word8, Word8, Word8, Control)
type DecodedData  = Maybe InstructionReg
type IssuedData   = Maybe (InstructionVal, [ Register ])
type ExecutedData = Maybe (Maybe (Register, Int32), Bool)

-- Define types for memories.
type InstMem = Seq Word8
type DataMem = Map Word32 Word8

-- |This data type contains all of the processors state.
data Processor = Processor
  { _fetchStage      :: Fetch
  , _decInputLatches :: Either FetchedData [ FetchedData ]
  , _decodeStage     :: Decode
  , _issInputLatches :: Either DecodedData [ DecodedData ]
  , _issueStage      :: Issue
  , _exeInputLatches :: Either IssuedData [ IssuedData ]
  , _executeStage    :: Execute
  , _wrbInputLatches :: Either ExecutedData [ ExecutedData ]
  , _writebackStage  :: Writeback
  , _invalidate      :: Bool
  , _btac            :: BTAC
  , _patternHistory  :: PatternHistory
  , _instMem         :: InstMem
  , _dataMem         :: DataMem
  , _regFile         :: RegisterFile
  , _simData         :: Simdata
  , _instCycles      :: Inst Register -> Int
  , _halted          :: Bool
  , _options         :: Options
  }
-- Let Template Haskell make the lenses.
makeLenses ''Processor

newProcessor :: [ Word8 ] -> Options -> Processor
newProcessor insts opts = Processor
  { _fetchStage      = newFetch
  , _decInputLatches = latches (_procType opts)
  , _decodeStage     = newDecode
  , _issInputLatches = latches (_procType opts)
  , _issueStage      = newIssue
  , _exeInputLatches = latches (_procType opts)
  , _executeStage    = newExecute
  , _wrbInputLatches = latches (_procType opts)
  , _writebackStage  = newWriteback
  , _invalidate      = False
  , _btac            = newBTAC
  , _patternHistory  = newPatternHistory
  , _instMem         = Seq.fromList insts
  , _dataMem         = Map.empty
  , _regFile         = newRegFile
  , _simData         = newSimdata
  , _instCycles      = defaultCycles
  , _halted          = False
  , _options         = opts
  }
  where latches Superscalar = Right . replicate (_noInstsPerCycle opts) $ Nothing
        latches _           = Left Nothing

-- Define the type of functions that operate on the processor.
type ProcessorState a = StateT Processor IO a

class HasItem a where
  item :: Word32 -> Lens' a Word8

instance HasItem InstMem where
  item i = lens (\mem   -> Seq.index mem (fromIntegral i))
                (\mem w -> Seq.update (fromIntegral i) w mem)

instance HasItem DataMem where
  item i = lens (\mem   -> Map.findWithDefault 0 i mem)
                (\mem w -> Map.insert i w mem)

condM :: (Monad m) => m Bool -> m a -> m a -> m a
condM mc m1 m2 = mc >>= (\c -> if c then m1 else m2)

whileM :: (Monad m) => m Bool -> m a -> m [a]
whileM mc mb = condM mc (do b <- mb
                            bs <- whileM mc mb
                            return (b:bs)) (return [])

whileM_ :: (Monad m) => m Bool -> m a -> m ()
whileM_ mc mb = whileM mc mb >> return ()
