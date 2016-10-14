module Compiler.Types
  ( ArgNo
  , Address
  , SPOffset
  , VariableMap
  , FunctionMap
  ) where

import Data.Map.Strict
import Compiler.SyntaxTree

type ArgNo       = Int
type Address     = Int
type SPOffset    = Int
type VariableMap = Map Identifier (Type, SPOffset)
type FunctionMap = Map Identifier (Type, [ Type ], [ Identifier ])
