module Main where

import System.Environment

import Compiler
import Assembler
import Simulator

main :: IO ()
main = do args <- getArgs
          case args of
            ("compile" :params) -> compiler_main  params
            ("assemble":params) -> assembler_main params
            ("simulate":params) -> simulator_main params
            _                   -> putStrLn usage_msg

usage_msg :: String
usage_msg =    "Usage: (" ++ compiler_msg  ++ ")"
          ++ "\n     | (" ++ assembler_msg ++ ")"
          ++ "\n     | (" ++ simulator_msg ++ ")"
