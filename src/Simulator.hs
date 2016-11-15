module Simulator
  ( simulator_main
  , simulator_msg
  ) where

import System.Exit
import System.FilePath
import System.Console.GetOpt
import Control.Monad
import qualified Data.ByteString.Lazy as BS

import Simulator.Data.Processor (Type(..))

data Flag = ProcType Type
          | Bypass
          | SubPipeline
          | Help
          deriving (Show, Eq, Read)

options :: [ OptDescr Flag ]
options
  = [ Option ['t'] ["proctype"]     (ReqArg (\str -> ProcType $ parseType str) "Processor Type")
      "Sets the type of processor to use, simple scalar, simple pipelined or superscalar."
    , Option ['b'] ["bypass"]       (NoArg Bypass)
      "Enables execution unit output bypassing."
    , Option ['s'] ["sub_pipeline"] (NoArg SubPipeline)
      "Enables sub pipelining in the processor."
    , Option []    ["help"]         (NoArg Help)
      "Prints this help message."
    ]

simulator_main :: [ String ] -> IO ()
simulator_main args = case getOpt Permute options args of
  (args, rp:fs,   []) -> if Help `elem` args
                           then putStrLn $ usageInfo header options
                           else do input <- BS.readFile rp
                                   let prog = BS.unpack input
                                   putStrLn "Not yet implemented."
                                   -- Get Options.
  (   _,     _, errs) -> do putStrLn (concat errs ++ usageInfo header options)
                            exitWith (ExitFailure 1)

parseType :: String -> Type
parseType "simple"      = Scalar
parseType "pipelined"   = Pipeline
parseType "superscalar" = Superscalar
parseType _             = undefined

header :: String
header = "Usage: " ++ simulator_msg

simulator_msg :: String
simulator_msg = "simulate [-t (simple | pipelined | superscalar)] [-b] [-s] read_path.o"
