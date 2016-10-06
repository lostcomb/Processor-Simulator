module Main where

import Assembly.Parser

main :: IO ()
main = do content <- readFile "test.asm"
          let tree = parseAssembly content
          putStrLn $ show tree
