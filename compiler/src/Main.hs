module Main where

import Assembler
import Interpreter
import Assembly.Parser

main :: IO ()
main = do content <- readFile "test.asm"
          interpret $ assemble $ parseAssembly content
