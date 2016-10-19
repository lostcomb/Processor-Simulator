-- |This module defines an interpreter for the simple instruction set.

module Interpreter
  ( interpret
  , interpretFile
  ) where

import Assembler
import Assembly.Parser
import Assembly.Instruction

import Data.Bits
import qualified Data.Map.Strict as Map

type Value     = Int
type Registers = [ Value ]

data State = State Registers (Map.Map Value Value)
  deriving (Show, Read)

-- |This function interprets the specified instructions and prints the result to
--  the terminal.
interpret :: [ Instruction ] -> IO ()
interpret = execute emptyState

-- |This function parses the contents of @path@ and interprets that. The result
--  is printed to the terminal.
interpretFile :: String -> IO ()
interpretFile path = do raw_insts <- readFile path
                        let insts = assemble . parseAssembly $ raw_insts
                        interpret insts

-- |This function executes the list of instructions, updating the state.
execute :: State -> [ Instruction ] -> IO ()
execute s@(State _ mem) insts =
  if length insts > fromIntegral (get_pc s)
    then (do let inst = insts !! fromIntegral (get_pc s)
                 ns   = executeInstruction s inst
             putStr "."
             execute ns insts
         )
    else (do putStrLn ""
             putStrLn "Result:"
             putStrLn $ "\tPC: " ++ show (get_pc s)
             mapM_ (\gpr -> putStrLn $  "\tGPR"
                                     ++ show gpr
                                     ++ ": "
                                     ++ show (get_gpr s gpr)) [0..15]
             putStrLn "\tMemory:"
             mapM_ (\(loc, val) -> putStrLn $  "\t\tMEM["
                                            ++ show loc
                                            ++ "] = "
                                            ++ show val) $ Map.assocs mem
             putStrLn ""
         )

-- |This constant defines the number of registers available.
registerNo :: Constant
registerNo = 100

-- |This constant defines an empty set of registers.
emptyRegisters :: Registers
emptyRegisters = replicate registerNo 0

-- |This constant defines an empty processor state.
emptyState :: State
emptyState = State emptyRegisters Map.empty

-- |This function executes @i@, returning the updated state.
executeInstruction :: State -> Instruction -> State
executeInstruction s i = case i of
  (ADD rd ri rj   ) -> inc_pc . set_gpr s rd $ get_gpr s ri + get_gpr s rj
  (SUB rd ri rj   ) -> inc_pc . set_gpr s rd $ get_gpr s ri - get_gpr s rj
  (MUL rd ri rj   ) -> inc_pc . set_gpr s rd $ get_gpr s ri * get_gpr s rj
  (DIV rd ri rj   ) -> inc_pc . set_gpr s rd $ get_gpr s ri `div` get_gpr s rj

  (AND rd ri rj   ) -> inc_pc . set_gpr s rd $ get_gpr s ri .&. get_gpr s rj
  (OR  rd ri rj   ) -> inc_pc . set_gpr s rd $ get_gpr s ri .|. get_gpr s rj
  (NOT rd ri      ) -> inc_pc . set_gpr s rd $ complement $ get_gpr s ri

  (JMP ri         ) -> set_pc s $ get_gpr s ri
  (BEZ (Left c) ri) -> if get_gpr s ri == 0
                         then set_pc s c
                         else inc_pc s

  (CEQ rd ri rj   ) -> inc_pc . set_gpr s rd $ if get_gpr s ri == get_gpr s rj
                                                 then 1
                                                 else 0
  (CGT rd ri rj   ) -> inc_pc . set_gpr s rd $ if get_gpr s ri >  get_gpr s rj
                                                 then 1
                                                 else 0

  (LDC rd (Left c)) -> inc_pc . set_gpr s rd $ c
  (LDM rd ri      ) -> inc_pc . set_gpr s rd $ get_mem s ri
  (STM ri rj      ) -> inc_pc . set_mem s ri $ rj

  (NOP            ) -> inc_pc s
  _                 -> error $  "Unable to execute Instruction.\n Current State: "
                             ++ show s
                             ++ "\n Instruction: "
                             ++ show i

-- |This function increments the program counter by 1.
inc_pc :: State -> State
inc_pc (State (pc:gprs) mem) = State ((pc + 1):gprs) mem

-- |This function sets the program counter to @npc@.
set_pc :: State -> Constant -> State
set_pc (State (pc:gprs) mem) npc = State (npc:gprs) mem

-- |This function reutrns the value of the program counter.
get_pc :: State -> Value
get_pc (State (pc:gprs) _) = pc

-- |This function sets the value of @gpr@ as @val@. If the program counter is
--  specified as @gpr@ 'error' is called.
set_gpr :: State -> Register -> Value -> State
set_gpr (State gprs mem) gpr val = State gprs' mem
  where (st, en) = if gpr == 0
                     then error $  "You can only get the value of the program "
                                ++ "counter using the JMP and BEZ Instructions."
                     else splitAt gpr gprs
        gprs'    = st ++ [val] ++ tail en

-- |This function returns the value of @gpr@.
get_gpr :: State -> Register -> Value
get_gpr (State gprs _) gpr = gprs !! gpr

-- |This function sets the memory location @loc@ to the value of @gpr@.
set_mem :: State -> Register -> Register -> State
set_mem s@(State gprs mem) loc gpr =
  State gprs $ Map.insert (get_gpr s loc) (get_gpr s gpr) mem

-- |This function returns the value at memory location @loc@.
get_mem :: State -> Register -> Value
get_mem s@(State _ mem) loc = Map.findWithDefault 0 (get_gpr s loc) mem
