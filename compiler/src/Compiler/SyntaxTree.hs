module Compiler.SyntaxTree
  ( Index
  , Identifier
  , Function(..)
  , Type(..)
  , Arg(..)
  , Statement(..)
  , DecVar(..)
  , Assign(..)
  , AssVar(..)
  , AssignDecl(..)
  , Expression(..)
  , FuncCall(..)
  ) where

type Index      = Int
type Identifier = String

data Function   = Function Type Identifier [ Arg ] [ Statement ]
                deriving (Show, Eq, Read)

data Type       = INT
                | BOOL
                deriving (Show, Eq, Read)

data Arg        = Arg Type Identifier
                deriving (Show, Eq, Read)

data Statement  = Declaration  DecVar
                | Assignment   Assign
                | AssignDeclr  AssignDecl
                | Cond         Expression [ Statement ] [ Statement ]
                | While        Expression [ Statement ]
                | For          (Maybe AssignDecl) (Maybe Expression) (Maybe Assign) [ Statement ]
                | FunctionCall FuncCall
                | Return       Expression
                deriving (Show, Eq, Read)

data DecVar     = DecVar Type Identifier (Maybe Expression)
                deriving (Show, Eq, Read)

data Assign     = Assign AssVar Expression
                deriving (Show, Eq, Read)

data AssVar     = AssVar Identifier (Maybe Expression)
                deriving (Show, Eq, Read)

data AssignDecl = AssignDecl DecVar Expression
                deriving (Show, Eq, Read)

data Expression = TRUE
                | FALSE
                | Const Int
                | Func  FuncCall
                | Var   AssVar
                | Add   Expression Expression
                | Sub   Expression Expression
                | Mul   Expression Expression
                | Div   Expression Expression
                | Eq    Expression Expression
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
