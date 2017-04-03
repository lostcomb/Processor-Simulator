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
-- |This module defines the syntax tree for the c-- language.
module Compiler.SyntaxTree (
  -- Type synonyms.
    Program
  , Identifier
  -- Types used for syntactic constructs in language.
  , Function(..)
  , Type(..)
  , Arg(..)
  , Variable(..)
  , Statement(..)
  , Expression(..)
  , FuncCall(..)
  ) where

type Size       = Int
type IsArray    = Bool
type Program    = [ Function ]
type Identifier = String

data Function   = Function Type Identifier [ Arg ] [ Statement ]
                deriving (Show, Eq, Read)

data Type       = INT
                | BOOL
                | VOID
                deriving (Show, Eq, Read)

data Arg        = Arg Type Identifier IsArray
                deriving (Show, Eq, Read)

data Variable   = Var     Identifier
                | Arr     Identifier Expression
                deriving (Show, Eq, Read)

data Statement  = Declaration  Type Variable
                | Assignment   Variable Expression
                | Cond         Expression [ Statement ] [ Statement ]
                | While        Expression [ Statement ]
                | FunctionCall FuncCall
                | Return       Expression
                deriving (Show, Eq, Read)

data Expression = TRUE
                | FALSE
                | Const Int
                | Func  FuncCall
                | EVar  Variable
                | Add   Expression Expression
                | Sub   Expression Expression
                | Mul   Expression Expression
                | Div   Expression Expression
                | Eq    Expression Expression
                | Neq   Expression Expression
                | Lt    Expression Expression
                | Gt    Expression Expression
                | Lte   Expression Expression
                | Gte   Expression Expression
                | Neg   Expression
                | And   Expression Expression
                | Or    Expression Expression
                deriving (Show, Eq, Read)

data FuncCall   = FuncCall Identifier [ Expression ]
                deriving (Show, Eq, Read)
