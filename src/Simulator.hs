{-
This file is part of aca-processor-simulator.

aca-processor-simulator is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

aca-processor-simulator is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with aca-processor-simulator.  If not, see <http://www.gnu.org/licenses/>.
-}
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
  = [ Option []    ["proctype"]
      (ReqArg (\t opts -> opts { _procType = parseType t }) "(scalar|pipelined|superscalar)")
      "Sets the type of processor to use, simple scalar, simple pipelined or superscalar."
    , Option ['b'] ["bypass"]
      (NoArg (\opts -> opts { _bypassEnabled = True }))
      "Enables execution unit output bypassing."
    , Option ['p'] ["sub_pipeline"]
      (NoArg (\opts -> opts { _pipelinedEUs = True }))
      "Enables sub pipelining in the processor."
    , Option []    ["branch_prediction"]
      (ReqArg (\t opts -> opts { _branchPrediction = parseBranchPrediction t }) "(static|saturating|twolevel)")
      "Sets the type of branch prediction to use, static (always not taken), saturating counter, two level adaptive."
    , Option ['k'] ["history_bits"]
      (ReqArg (\t opts -> opts { _branchHistoryBits = read t }) "1,2,..")
      "Sets the number of history bits to be used for each branch instruction."
    , Option ['e'] ["no_eus"]
      (ReqArg (\n opts -> opts { _noEUs = (read n) }) "1,2,..")
      "Sets the number of execution units to be used in a superscalar configuration."
    , Option ['f'] ["no_insts"]
      (ReqArg (\n opts -> opts { _noInstsPerCycle = (read n) }) "1,2,..")
      "Sets the number of instructions to be fetched per cycle."
    , Option ['o'] ["out_of_order"]
      (NoArg (\opts -> opts { _outOfOrder = True }))
      "Enables out-of-order execution."
    , Option ['u'] ["unaligned_issue"]
      (NoArg (\opts -> opts { _unAlignedIssue = True }))
      "Enables the use of an unaligned issue window."
    , Option ['i'] ["window_size"]
      (ReqArg (\n opts -> opts { _issueWindowSize = read n }) "1,2,..")
      "Sets the number of instructions to choose from when issuing."
    , Option ['s'] ["shelf_size"]
      (ReqArg (\n opts -> opts { _shelfSize = read n }) "1,2,..")
      "Sets the maximum number of instructions a reservation station can hold."
    , Option ['r'] ["rob_size"]
      (ReqArg (\n opts -> opts { _robSize = read n }) "1,2,..")
      "Sets the number of entries in the reorder buffer."
    , Option []    ["nehalem"]
      (NoArg (\_ -> nehalemOptions ))
      "Uses the default options for an Intel nehalem type processor."
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
                                   evalStateT (evalStateT runProcessor []) $ newProcessor prog opts
  (_,     _, errs) -> do putStrLn (concat errs ++ usageInfo header optionList)
                         exitWith (ExitFailure 1)

parseType :: String -> Type
parseType "scalar"      = Scalar
parseType "pipelined"   = Pipelined
parseType "superscalar" = Superscalar
parseType _             = undefined

parseBranchPrediction :: String -> BranchPrediction
parseBranchPrediction "static"     = Static
parseBranchPrediction "saturating" = Saturating
parseBranchPrediction "twolevel"   = TwoLevel
parseBranchPrediction _            = undefined

header :: String
header = "Usage: " ++ simulator_msg

simulator_msg :: String
simulator_msg = "simulate [options (use --help to list)] read_path.o"
