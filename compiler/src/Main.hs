module Main where

import Assembler
import Interpreter
import Assembly.Parser

import Compiler.Parser
import Compiler.Analyse
import Compiler.Generator

import System.Environment

dispatch :: [ (String, String -> String) ]
dispatch =  [ ("-parse"   , show .                                 parseStr)
            , ("-analyse" , show .                       analyse . parseStr)
            , ("-generate", show .            generate . analyse . parseStr)
            , ("-assemble", show . assemble . generate . analyse . parseStr)
            ]

main :: IO ()
main = do (command:args) <- getArgs
          if command == "-interpret"
            then readFile (head args) >>= interpret
                                      .   assemble
                                      .   generate
                                      .   analyse
                                      .   parseStr
            else exec (lookup command dispatch) args
  where exec (Just f) (r_p:w_p:_) = do c <- readFile r_p
                                       writeFile w_p $ f c
        exec (Just f) (r_p    :_) = do c <- readFile r_p
                                       putStrLn $ f c
        exec _ _ = error $  "Usage: compiler (-parse | -analyse | "
                               ++ "-generate | -assemble | -interpret) "
                               ++ "file_path.cmm"
