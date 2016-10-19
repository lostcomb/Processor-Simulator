module Main where

import Assembler
import Interpreter
import Assembly.Parser

import Compiler.Parser
import Compiler.Analyse
import Compiler.Generator

import System.Environment

main :: IO ()
main = do args <- getArgs
          case args of
            [ file_path ] -> compile file_path
            _             -> error "Usage: compiler file_path.c--"

compile :: FilePath -> IO ()
compile file_path = do contents <- readFile file_path
                       let insts = assemble . generate . analyse . parseStr $ contents
                       writeFile (file_path ++ ".asm") $ show insts
                       interpret insts
