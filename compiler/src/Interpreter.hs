module Interpreter
  ( interpret
  , interpretFile
  ) where

import Assembly.Parser
import Assembly.Instruction

import Data.Bits
import Data.Word
import qualified Data.Map as Map

data State     = State Registers (Map.Map Word32 Word32)
  deriving (Show, Read)
data Registers = Registers Word32 [ Word32 ]
  deriving (Show, Read)


interpret :: [ Instruction ] -> IO ()
interpret insts = undefined--TODO

interpretFile :: String -> IO ()
interpretFile path = do raw_insts <- readFile path
                        let insts = assemble . parseAssembly raw_insts
                        -- TODO

-- TODO: Need a function which executes the correct instruction based upon the current pc value.

emptyRegisters :: Registers
emptyRegisters = Registers 0 [ 0 | x <- 0..15 ]

emptyState :: State
emptyState = State emptyRegisters Map.empty

executeInstruction :: State -> Instruction -> State
executeInstruction s i = case i of
  (ADD rd       ri rj) -> inc_pc . set s rd $ get s ri + get s rj
  (SUB rd       ri rj) -> inc_pc . set s rd $ get s ri - get s rj
  (MUL rd       ri rj) -> inc_pc . set s rd $ get s ri * get s rj
  (DIV rd       ri rj) -> inc_pc . set s rd $ get s ri `div` get s rj
  (AND rd       ri rj) -> inc_pc . set s rd $ get s ri .&. get s rj
  (OR  rd       ri rj) -> inc_pc . set s rd $ get s ri .|. get s rj
  (XOR rd       ri rj) -> inc_pc . set s rd $ get s ri `xor` get s rj
  (NOT rd       ri   ) -> inc_pc . set s rd $ complement $ get s ri
  (JMP          ri   ) -> set_pc s $ get s ri
  (BEQ (Left c) ri rj) -> if get s ri == get s rj then set_pc s c else inc_pc s
  (BGT (Left c) ri rj) -> if get s ri >  get s rj then set_pc s c else inc_pc s
  (BEZ (Left c) ri   ) -> if get s ri == 0        then set_pc s c else inc_pc s
  (LDC rd    c       ) -> inc_pc . set s rd $ c
  (LDM rd       ri   ) -> inc_pc . set s rd $ get_mem ri
  (STM ri          rj) -> inc_pc . set_mem s ri $ rj
  (NOP               ) -> inc_pc s
  _                    -> error $ "Unable to execute Instruction.\n Current State: " ++ show s ++ "\n Instruction: " ++ show i

inc_pc :: State -> State
inc_pc (State (Registers pc gprs) mem) = State (Registers (pc + 1) gprs) mem

set_pc :: State -> Word32 -> State
set_pc (State (Registers _ gprs) mem) pc = State (Registers pc gprs) mem

set :: State -> Word32 -> Word32 -> State
set (State (Registers pc gprs) mem) gpr val = undefined--TODO

get :: State -> Word32 -> Word32
get (State (Registers pc gprs) mem) gpr = undefined--TODO

set_mem :: State -> Word32 -> Word32 -> State
set_mem (State (Registers pc gprs) mem) loc gpr = undefined--TODO

get_mem :: State -> Word32 -> Word32
get_mem (State r mem) loc = undefined--TODO
