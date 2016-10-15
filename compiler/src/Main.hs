module Main where

import Assembler
import Interpreter
import Assembly.Parser

import Compiler.Parser
import Compiler.Analyse
import Compiler.Generator

main :: IO ()
--main = do content <- readFile "test.asm"
--          interpret $ assemble $ parseAssembly content
main = do source <- readFile "src/tests_c--/syntax.c--"
          putStrLn $ show $ parseStr source