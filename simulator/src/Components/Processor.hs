module Components.Processor
  ( Processor(..)
  , CPU(..)
  ) where

import Data.Int
import Data.Word
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State
import Components.Simdata
import Components.Registers
import Components.Instructions
import Components.RegisterFile

-- |This data type contains all of the processors state.
data Processor f d i e w = Processor
  { fetch_stage       :: f
  , dec_input_latches :: [ (Word8, Word8, Word8, Word8) ]
  , decode_stage      :: d
  , iss_input_latches :: [ Instruction Int ]
  , issue_stage       :: i
  , exe_input_latches :: [ Instruction Int ]
  , execute_stage     :: e
  , wrb_input_latches :: [ (Register, Int32) ]
  , writeback_stage   :: w
  , inst_mem          :: [ Word8 ]
  , data_mem          :: Map Word32 Word8
  , reg_file          :: RegisterFile
  , sim_data          :: Simdata
  }

-- |This class defines the functions to read and write a CPUs state.
class CPU proc where
  -- Register functions.
  getRegFile         :: State (proc f d i e w) RegisterFile
  -- Memory functions.
  getInstMemItem     :: Word32 -> State (proc f d i e w) Word8
  getDataMemItem     :: Word32 -> State (proc f d i e w) Word8
  setDataMemItem     :: Word32 -> Word8 -> State (proc f d i e w) ()
  getDataMemLocs     :: State (proc f d i e w) [ Word32 ]
  -- Input latch functions.
  getDecInputLatches :: State (proc f d i e w) [ (Word8, Word8, Word8, Word8) ]
  setDecInputLatches :: [ (Word8, Word8, Word8, Word8) ] -> State (proc f d i e w) ()
  getIssInputLatches :: State (proc f d i e w) [ Instruction Int ]
  setIssInputLatches :: [ Instruction Int ] -> State (proc f d i e w) ()
  getExeInputLatches :: State (proc f d i e w) [ Instruction Int ]
  setExeInputLatches :: [ Instruction Int ] -> State (proc f d i e w) ()
  getWrbInputLatches :: State (proc f d i e w) [ (Register, Int32) ]
  setWrbInputLatches :: [ (Register, Int32) ] -> State (proc f d i e w) ()
  -- Stage functions.
  getFetch           :: State (proc f d i e w) f
  setFetch           :: f -> State (proc f d i e w) ()
  getDecode          :: State (proc f d i e w) d
  setDecode          :: d -> State (proc f d i e w) ()
  getIssue           :: State (proc f d i e w) i
  setIssue           :: i -> State (proc f d i e w) ()
  getExecute         :: State (proc f d i e w) e
  setExecute         :: e -> State (proc f d i e w) ()
  getWriteback       :: State (proc f d i e w) w
  setWriteback       :: w -> State (proc f d i e w) ()

-- |Define the Processor types CPU instance.
instance CPU Processor where
  -- Register functions.
  getRegFile = state $ \s -> (reg_file s, s)
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
  -- State functions.
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
