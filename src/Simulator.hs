module Simulator
  ( simulator_main
  , simulator_msg
  ) where

import System.Exit
import System.FilePath
import System.Console.GetOpt
import Control.Monad
import Control.Monad.State
import qualified Data.ByteString.Lazy as BS

import Simulator.Simulator
import Simulator.Data.Processor

optionList :: [ OptDescr (Options -> Options) ]
optionList
  = [ Option ['t'] ["proctype"]
      (ReqArg (\t opts -> opts { _procType = parseType t }) "(scalar|pipelined|superscalar)")
      "Sets the type of processor to use, simple scalar, simple pipelined or superscalar."
    , Option ['b'] ["bypass"]
      (NoArg (\opts -> opts { _bypassEnabled = True }))
      "Enables execution unit output bypassing."
    , Option ['s'] ["sub_pipeline"]
      (NoArg (\opts -> opts { _pipelinedEUs = True }))
      "Enables sub pipelining in the processor."
    , Option []    ["help"]
      (NoArg (\opts -> opts { help = True }))
      "Prints this help message."
    ]

simulator_main :: [ String ] -> IO ()
simulator_main args = case getOpt Permute optionList args of
  (o, rp:fs,   []) -> do let opts = foldl (flip id) defaultOptions o
                         if help opts
                           then putStrLn $ usageInfo header optionList
                           else do input <- BS.readFile rp
                                   let prog = BS.unpack input
                                   evalStateT runProcessor $ newProcessor prog 1 1 opts --TODO: Update to allow user set params.
  (   _,     _, errs) -> do putStrLn (concat errs ++ usageInfo header optionList)
                            exitWith (ExitFailure 1)

parseType :: String -> Type
parseType "scalar"      = Scalar
parseType "pipelined"   = Pipelined
parseType "superscalar" = Superscalar
parseType _             = undefined

header :: String
header = "Usage: " ++ simulator_msg

simulator_msg :: String
simulator_msg = "simulate [-t (simple | pipelined | superscalar)] [-b] [-s] read_path.o"
