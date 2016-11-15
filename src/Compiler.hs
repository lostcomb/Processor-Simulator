module Compiler
  ( compiler_main
  , compiler_msg
  ) where

import Compiler.Parser
import Compiler.Analyser
import Compiler.Generator
import Compiler.Allocator

import System.Exit
import System.FilePath
import System.Console.GetOpt
import Control.Monad

data Flag = Parse
          | Generate
          | Help
          deriving (Show, Eq, Read)

options :: [ OptDescr Flag ]
options
  = [ Option ['p'] ["parse"]    (NoArg Parse)
      "Outputs the abstract syntax tree generated by the parser."
    , Option ['g'] ["generate"] (NoArg Generate)
      "Outputs the generated assembly code."
    , Option []    ["help"]      (NoArg Help)
      "Prints this help message."
    ]

compiler_main :: [ String ] -> IO ()
compiler_main args = case getOpt Permute options args of
  (args, rp:fs,   []) -> if Help `elem` args
                           then putStrLn $ usageInfo header options
                           else do input <- readFile rp
                                   let parsed    = parse input
                                       analysed  = analyse parsed
                                       generated = generate analysed
                                       allocated = allocate generated
                                   when (Parse `elem` args) $
                                     writeFile (replaceExtension rp ".cmm_syn")
                                               (show parsed)
                                   when (Generate `elem` args) $
                                     writeFile (replaceExtension rp ".gen.asm")
                                               (unlines . map show $ generated)
                                   writeFile (getWritePath rp fs) $
                                             unlines . map show $ allocated
  (   _,     _, errs) -> do putStrLn (concat errs ++ usageInfo header options)
                            exitWith (ExitFailure 1)

getWritePath :: String -> [ String ] -> FilePath
getWritePath _  (wp:[]) = wp
getWritePath rp _       = replaceExtension rp ".asm"

header :: String
header = "Usage: " ++ compiler_msg

compiler_msg :: String
compiler_msg = "compile [-p] [-g] read_path.cmm [write_path.asm]"
