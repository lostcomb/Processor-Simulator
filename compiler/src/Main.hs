module Main where

import Assembler
import Assembly.Parser

main :: IO ()
main = do content <- readFile "test.asm"
          let tree = parseAssembly content
          putStrLn $ show tree
          let insts = assemble tree
              bin   = assembleBinary tree
          putStrLn $ show insts
          putStrLn $ show bin
