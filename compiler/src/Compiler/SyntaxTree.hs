module Compiler.SyntaxTree
  ( Index
  , Identifier
  , Arg(..)
  , Declare(..)
  , Assign(..)
  , AssignDecl(..)
  , Statement(..)
  , FuncCall(..)
  , Function(..)
  , Aexp(..)
  , Bexp(..)
  ) where

type Index      = Int
type Identifier = String

data Arg        = ArgInt  Identifier
                | ArgBool Identifier
                deriving (Show, Eq, Read)

data Declare    = DeclareInt  Identifier Index
                | DeclareBool Identifier Index
                deriving (Show, Eq, Read)

data Assign     = AssignAexp Identifier Index Aexp
                | AssignBexp Identifier Index Bexp
                deriving (Show, Eq, Read)

data AssignDecl = AssignDeclInt  Identifier Index Aexp
                | AssignDeclBool Identifier Index Bexp
                deriving (Show, Eq, Read)

data Statement  = Declaration  Declare
                | Assignment   Assign
                | AssignDeclr  AssignDecl
                | Cond         Bexp [ Statement ] [ Statement ]
                | While        Bexp [ Statement ]
                | For          (Maybe AssignDecl) (Maybe Bexp) (Maybe Assign) [ Statement ]
                | FunctionCall FuncCall
                | ReturnAexp   Aexp
                | ReturnBexp   Bexp
                deriving (Show, Eq, Read)

data FuncCall   = FuncCall Identifier [ (Either Aexp Bexp) ]
                deriving (Show, Eq, Read)

data Function   = FunctionInt  Identifier [ Arg ] [ Statement ]
                | FunctionBool Identifier [ Arg ] [ Statement ]
                | FunctionVoid Identifier [ Arg ] [ Statement ]
                deriving (Show, Eq, Read)

data Aexp       = Const Int
                | Var   Identifier Index
                | Func  FuncCall
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
