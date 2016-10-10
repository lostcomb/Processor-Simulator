module Compiler.SyntaxTree
  ( Index
  , Identifier
  , Expression
  , OptionalAexp
  , Function(..)
  , Type(..)
  , Arg(..)
  , Statement(..)
  , DecVar(..)
  , Assign(..)
  , AssVar(..)
  , AssignDecl(..)
  , Aexp(..)
  , Bexp(..)
  , FuncCall(..)
  ) where

type Index      = Int
type Identifier = String
type Expression = Either Aexp Bexp
type OptionalAexp = Maybe Aexp

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
                | Cond         Bexp [ Statement ] [ Statement ]
                | While        Bexp [ Statement ]
                | For          (Maybe AssignDecl) (Maybe Bexp) (Maybe Assign) [ Statement ]
                | FunctionCall FuncCall
                | Return       Expression
                deriving (Show, Eq, Read)

data DecVar     = DecVar Type Identifier OptionalAexp
                deriving (Show, Eq, Read)

data Assign     = Assign AssVar Expression
                deriving (Show, Eq, Read)

data AssVar     = AssVar Identifier OptionalAexp
                deriving (Show, Eq, Read)

data AssignDecl = AssignDecl DecVar Expression
                deriving (Show, Eq, Read)

data Aexp       = Const Int
                | Func  FuncCall
                | Var   AssVar
                | Add   Aexp Aexp
                | Sub   Aexp Aexp
                | Mul   Aexp Aexp
                | Div   Aexp Aexp
                deriving (Show, Eq, Read)

data Bexp       = TRUE
                | FALSE
                | Eq  Aexp Aexp
                | Lt  Aexp Aexp
                | Gt  Aexp Aexp
                | Lte Aexp Aexp
                | Gte Aexp Aexp
                | Neg Bexp
                | And Bexp Bexp
                | Or  Bexp Bexp
                deriving (Show, Eq, Read)

data FuncCall   = FuncCall Identifier [ Expression ]
                deriving (Show, Eq, Read)
