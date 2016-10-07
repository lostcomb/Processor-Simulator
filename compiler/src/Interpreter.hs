module Interpreter
  ( interpret
  , interpretFile
  ) where

import Assembler
import Assembly.Parser
import Assembly.Instruction

import Data.Bits
import Data.Word
import qualified Data.Map.Strict as Map

type Value = Word32

data Registers = Registers Word32 [ Word32 ]
  deriving (Show, Read)
data State = State Registers (Map.Map Word32 Word32)
  deriving (Show, Read)

interpret :: [ Instruction ] -> IO ()
interpret = execute emptyState

interpretFile :: String -> IO ()
interpretFile path = do raw_insts <- readFile path
                        let insts = assemble . parseAssembly $ raw_insts
                        interpret insts

execute :: State -> [ Instruction ] -> IO ()
execute s@(State _ mem) insts = if length insts >= fromIntegral (get_pc s)
                                  then (do let inst = insts !! fromIntegral (get_pc s)
                                               ns   = executeInstruction s inst
                                           putStr "."
                                           execute ns insts
                                       )
                                  else (do putStrLn ""
                                           putStrLn "Result:"
                                           putStrLn $ "\tPC: " ++ show (get_pc s)
                                           mapM_ (\gpr -> putStrLn $ "\tGPR" ++ show gpr ++ ": " ++ show (get s gpr)) [0..15]
                                           putStrLn "\tMemory:"
                                           mapM_ (\(loc, val) -> putStrLn $ "\t\tMEM[" ++ show loc ++ "] = " ++ show val) $ Map.assocs mem
                                           putStrLn ""
                                       )

emptyRegisters :: Registers
emptyRegisters = Registers 0 [ 0 | x <- [0..15] ]

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
  (LDM rd       ri   ) -> inc_pc . set s rd $ get_mem s ri
  (STM ri          rj) -> inc_pc . set_mem s ri $ rj
  (NOP               ) -> inc_pc s
  _                    -> error $ "Unable to execute Instruction.\n Current State: " ++ show s ++ "\n Instruction: " ++ show i

inc_pc :: State -> State
inc_pc (State (Registers pc gprs) mem) = State (Registers (pc + 1) gprs) mem

set_pc :: State -> Constant -> State
set_pc (State (Registers _ gprs) mem) pc = State (Registers pc gprs) mem

get_pc :: State -> Value
get_pc (State (Registers pc _) _) = pc

set :: State -> Register -> Value -> State
set (State (Registers pc gprs) mem) gpr val = State (Registers pc ugprs) mem
  where (st, en) = splitAt (fromIntegral gpr) gprs
        ugprs    = st ++ [val] ++ tail en

get :: State -> Register -> Value
get (State (Registers _ gprs) _) gpr = gprs !! fromIntegral gpr

set_mem :: State -> Register -> Register -> State
set_mem s@(State r mem) loc gpr = State r $ Map.insert (get s loc) (get s gpr) mem

get_mem :: State -> Register -> Value
get_mem s@(State _ mem) loc = Map.findWithDefault 0 (get s loc) mem
