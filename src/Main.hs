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
module Main where

import System.Environment

import Compiler
import Assembler
import Simulator

main :: IO ()
main = do args <- getArgs
          case args of
            ("compile" :params) -> compiler_main  params
            ("assemble":params) -> assembler_main params
            ("simulate":params) -> simulator_main params
            _                   -> putStrLn usage_msg

usage_msg :: String
usage_msg =    "Usage: (" ++ compiler_msg  ++ ")"
          ++ "\n     | (" ++ assembler_msg ++ ")"
          ++ "\n     | (" ++ simulator_msg ++ ")"
