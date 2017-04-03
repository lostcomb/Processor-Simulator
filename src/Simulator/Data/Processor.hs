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
  , module Simulator.Data.ReservationStation
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
import Simulator.Data.ReservationStation

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
  , _unAlignedIssue    :: Bool
  , _issueWindowSize   :: Int
  , _shelfSize         :: Int
  , _robSize           :: Int
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
  , _branchHistoryBits = 2
  , _noEUs             = 1
  , _noInstsPerCycle   = 1
  , _outOfOrder        = False
  , _unAlignedIssue    = False
  , _issueWindowSize   = 4
  , _shelfSize         = 4
  , _robSize           = 28
  , help               = False
  }

nehalemOptions :: Options
nehalemOptions = Options
  { _procType          = Superscalar
  , _bypassEnabled     = True
  , _pipelinedEUs      = True
  , _branchPrediction  = TwoLevel
  , _branchHistoryBits = 2
  , _noEUs             = 6
  , _noInstsPerCycle   = 4
  , _outOfOrder        = True
  , _unAlignedIssue    = True
  , _issueWindowSize   = 4
  , _shelfSize         = 4
  , _robSize           = 28
  , help               = False
  }

-- |These types correspond to the types of the output data for each stage.
type FetchedData  = Maybe (Word8, Word8, Word8, Word8, Control)
type DecodedData  = Maybe InstructionReg
type IssuedData   = Maybe (Int, InstructionVal, [ Register ])
type ExecutedData = Maybe (Int, Maybe (Register, Int32), Bool)

-- Define types for memories.
type InstMem = Seq Word8
type DataMem = Map Word32 Word8

-- |This data type contains all of the processors state.
data Processor = Processor
  { _fetchStage          :: Fetch
  , _decInputLatches     :: Either FetchedData [ FetchedData ]
  , _decodeStage         :: Decode
  , _robInputLatches     :: ([ DecodedData ], [ ExecutedData ])
  , _robStage            :: ReOrderBuffer
  , _issInputLatches     :: Either DecodedData [ DecodedData ]
  , _issueStage          :: Issue
  , _exeInputLatches     :: Either IssuedData [ IssuedData ]
  , _executeStage        :: Execute
  , _wrbInputLatches     :: Either ExecutedData [ ExecutedData ]
  , _writebackStage      :: Writeback
  , _invalidate          :: Bool
  , _btac                :: BTAC
  , _patternHistory      :: PatternHistory
  , _reservationStations :: [ ReservationStation ]
  , _registerAliasTable  :: RegisterAliasTable
  , _instMem             :: InstMem
  , _dataMem             :: DataMem
  , _regFile             :: RegisterFile
  , _simData             :: Simdata
  , _instCycles          :: Inst Register -> Int
  , _halted              :: Bool
  , _options             :: Options
  }
-- Let Template Haskell make the lenses.
makeLenses ''Processor

newProcessor :: [ Word8 ] -> Options -> Processor
newProcessor insts opts = Processor
  { _fetchStage          = newFetch
  , _decInputLatches     = latches (_procType opts)
  , _decodeStage         = newDecode
  , _robInputLatches     = ([], [])
  , _robStage            = newReOrderBuffer
  , _issInputLatches     = latches (_procType opts)
  , _issueStage          = newIssue
  , _exeInputLatches     = eLatches (_procType opts)
  , _executeStage        = newExecute (_noEUs opts)
  , _wrbInputLatches     = latches (_procType opts)
  , _writebackStage      = newWriteback
  , _invalidate          = False
  , _btac                = newBTAC
  , _patternHistory      = newPatternHistory
  , _reservationStations = replicate (_noEUs opts) rs
  , _registerAliasTable  = newRegisterAliasTable
  , _instMem             = Seq.fromList insts
  , _dataMem             = Map.empty
  , _regFile             = newRegFile
  , _simData             = newSimdata
  , _instCycles          = defaultCycles
  , _halted              = False
  , _options             = opts
  }
  where eLatches Superscalar = Right . replicate (_noEUs opts) $ Nothing
        eLatches _           = Left Nothing
        latches  Superscalar = Right . replicate (_noInstsPerCycle opts) $ Nothing
        latches  _           = Left Nothing
        rs                   = newReservationStation (_shelfSize opts)

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
