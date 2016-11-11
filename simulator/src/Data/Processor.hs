module Data.Processor
  ( Processor(..)
  , Processor_S(..)
  , Fetch(..)
  , Decode(..)
  , Issue(..)
  , Execute(..)
  , RegisterFile(..)
  , pc
  ) where

import Data.Int
import Data.Word
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State

import Data.Simdata
import Data.Registers
import Data.Instruction
import Data.Stage.Fetch
import Data.Stage.Decode
import Data.Stage.Issue
import Data.Stage.Execute
import Data.Stage.Writeback

-- |This data type contains all of the processors state.
data Processor_S = Processor_S
  { fetch_stage       :: Fetch_T
  , dec_input_latches :: [ Maybe (Word8, Word8, Word8, Word8) ]
  , decode_stage      :: Decode_T
  , iss_input_latches :: [ Maybe InstructionReg ]
  , issue_stage       :: Issue_T
  , exe_input_latches :: [ Maybe InstructionVal ]
  , execute_stage     :: Execute_T
  , wrb_input_latches :: [ Maybe (Register, Int32) ]
  , writeback_stage   :: Writeback_T
  , inst_mem          :: [ Word8 ]
  , data_mem          :: Map Word32 Word8
  , reg_file          :: RegisterFile_T
  , sim_data          :: Simdata
  , inst_cycles       :: Inst Register -> Int
  , options           :: Processor_O
  }

data Processor_O = Processor_O
  { proc_type      :: Processor_T
  , bypass_enabled :: Bool
  }

data Processor_T = Scalar
                 | Pipeline
                 | Superscalar
                 deriving (Show, Eq, Read)

-- |This class defines the functions to read and write a CPUs state.
class Processor proc where
  -- Register functions.
  getRegFile         :: State proc RegisterFile_T
  setRegFile         :: RegisterFile_T -> State proc ()
  -- Memory functions.
  getInstMemItem     :: Word32 -> State proc Word8
  getDataMemItem     :: Word32 -> State proc Word8
  setDataMemItem     :: Word32 -> Word8 -> State proc ()
  getDataMemLocs     :: State proc [ Word32 ]
  -- Input latch functions.
  getDecInputLatches :: State proc [ Maybe (Word8, Word8, Word8, Word8) ]
  setDecInputLatches :: [ Maybe (Word8, Word8, Word8, Word8) ] -> State proc ()
  getIssInputLatches :: State proc [ Maybe InstructionReg ]
  setIssInputLatches :: [ Maybe InstructionReg ] -> State proc ()
  getExeInputLatches :: State proc [ Maybe InstructionVal ]
  setExeInputLatches :: [ Maybe InstructionVal ] -> State proc ()
  getWrbInputLatches :: State proc [ Maybe (Register, Int32) ]
  setWrbInputLatches :: [ Maybe (Register, Int32) ] -> State proc ()
  -- Stage functions.
  getFetch           :: State proc Fetch_T
  setFetch           :: Fetch_T -> State proc ()
  getDecode          :: State proc Decode_T
  setDecode          :: Decode_T -> State proc ()
  getIssue           :: State proc Issue_T
  setIssue           :: Issue_T -> State proc ()
  getExecute         :: State proc Execute_T
  setExecute         :: Execute_T -> State proc ()
  getWriteback       :: State proc Writeback_T
  setWriteback       :: Writeback_T -> State proc ()

-- |Define the Processor types CPU instance.
instance Processor Processor_S where
  -- Register functions.
  getRegFile   = state $ \s -> (reg_file s, s)
  setRegFile r = state $ \s -> ((), s { reg_file = r })
  -- Memory functions.
  getInstMemItem l   = state $ \s -> (genericIndex (inst_mem s) l, s)
  getDataMemItem l   = state $ \s -> (Map.findWithDefault 0 l (data_mem s), s)
  setDataMemItem l v = state $ \s -> ((), s { data_mem = Map.insert l v (data_mem s) })
  getDataMemLocs     = state $ \s -> (Map.keys (data_mem s), s)
  -- Input latch functions.
  getDecInputLatches    = state $ \s -> (dec_input_latches s, s)
  setDecInputLatches ls = state $ \s -> ((), s { dec_input_latches = ls })
  getIssInputLatches    = state $ \s -> (iss_input_latches s, s)
  setIssInputLatches ls = state $ \s -> ((), s { iss_input_latches = ls })
  getExeInputLatches    = state $ \s -> (exe_input_latches s, s)
  setExeInputLatches ls = state $ \s -> ((), s { exe_input_latches = ls })
  getWrbInputLatches    = state $ \s -> (wrb_input_latches s, s)
  setWrbInputLatches ls = state $ \s -> ((), s { wrb_input_latches = ls })
  -- Stage functions.
  getFetch       = state $ \s -> (fetch_stage s, s)
  setFetch     f = state $ \s -> ((), s { fetch_stage = f })
  getDecode      = state $ \s -> (decode_stage s, s)
  setDecode    d = state $ \s -> ((), s { decode_stage = d })
  getIssue       = state $ \s -> (issue_stage s, s)
  setIssue     i = state $ \s -> ((), s { issue_stage = i })
  getExecute     = state $ \s -> (execute_stage s, s)
  setExecute   e = state $ \s -> ((), s { execute_stage = e })
  getWriteback   = state $ \s -> (writeback_stage s, s)
  setWriteback w = state $ \s -> ((), s { writeback_stage = w })

instance RegisterFile Processor_S where
  getRegs    = do reg_file <- getRegFile
                  return $ Map.toList reg_file
  getReg r   = do reg_file <- getRegFile
                  return $ Map.findWithDefault 0 r reg_file
  setReg r v = do reg_file <- getRegFile
                  setRegFile $ Map.insert r v reg_file

instance Fetch Processor_S where
  getNoOfInsts        = do f <- getFetch
                           return $ _no_of_insts f
  getProgramCounter   = do f <- getFetch
                           return $ _program_counter f
  setProgramCounter n = do f <- getFetch
                           setFetch $ f { _program_counter = n }

instance Decode Processor_S where
  getInstructionCycles i = state $ \s -> (inst_cycles s i, s)

instance Issue Processor_S where
  getIssueWindow   = do i <- getIssue
                        return $ _issue_window i
  setIssueWindow w = do i <- getIssue
                        setIssue $ i { _issue_window = w }
  getExecWindow    = do i <- getIssue
                        return $ _exec_window i
  setExecWindow  w = do i <- getIssue
                        setIssue $ i { _exec_window = w }

instance Execute Processor_S where
  getBypassValues     = do e <- getExecute
                           return $ _bypass_values e
  setBypassValues   b = do e <- getExecute
                           setExecute $ e { _bypass_values = b }
  getNoExecutionUnits = do e <- getExecute
                           return $ _no_execution_units e
