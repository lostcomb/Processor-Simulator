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
