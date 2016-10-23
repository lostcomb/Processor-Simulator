-- |This module analyses a program to make sure that types are correct, and
--  that all used variable and functions are declared.

module Compiler.Analyser
  ( analyse
  ) where

import Compiler.SyntaxTree

import Data.List (foldl')
import Control.Monad ((<$!>))

type ArrayT        = (Identifier, Type )
type VariableT     = (Identifier, Type )
type FunctionT     = (Identifier, Type )
type FunctionArgsT = (Identifier, [Arg])
type Types         = ([ArrayT], [VariableT], [FunctionT], [FunctionArgsT])

-- |This function analyses the specified progam @prog@.
analyse :: Program -> Program
analyse prog = vs'' `seq` prog
  where vs'  = foldl' (\(a, v, f, fa) (Function t i args stmts) ->
                                                (a, v, (i, t):f, (i, args):fa))
                     ([], [], [], []) prog
        vs'' = foldl' analyseFunction vs' prog

-- |This function analyses the specified function.
analyseFunction :: Types -> Function -> Types
analyseFunction vs (Function t i args stmts) = vs'' `seq` hasRet `seq` vs
  where vs'    = foldl' addArg vs args
        vs''   = analyseStatements i vs' stmts
        hasRet = foldl' (\b s -> if isRet s then True else b) False stmts
        isRet (Return _) = True
        isRet (       _) = False
        addArg (a, v, f, fa) (Arg t i True ) = ((i, t):a, v, f, fa)
        addArg (a, v, f, fa) (Arg t i False) = (a, (i, t):v, f, fa)

-- |This function analyses the specified statments @stmts@.
analyseStatements :: Identifier -> Types -> [ Statement ] -> Types
analyseStatements f_i vs stmts = foldl' (analyseStatement f_i) vs stmts

-- |This function analyses the specified statment @s@.
analyseStatement :: Identifier -> Types -> Statement -> Types
analyseStatement f_i vs@(a, v, f, fa) s = case s of
  (Declaration  t (Var i   )  ) -> (a, (i, t):v, f, fa)
  (Declaration  t (Arr i _ )  ) -> ((i, t):a, v, f, fa)
  (Assignment     (Var i   ) e) -> checkType (exprType vs e) (get v i) `seq` vs
  (Assignment     (Arr i ex) e) -> checkType (exprType vs ex) INT `seq`
                                   checkType (exprType vs e) (get a i) `seq` vs
  (Cond         e s1 s2       ) -> checkType (exprType vs e) BOOL `seq`
                                   analyseStatements f_i vs s1 `seq`
                                   analyseStatements f_i vs s2 `seq`
                                   vs
  (While        e st          ) -> checkType (exprType vs e) BOOL `seq`
                                   analyseStatements f_i vs st `seq`
                                   vs
  (FunctionCall func          ) -> checkFuncCall vs func `seq` vs
  (Return       e             ) -> checkType (exprType vs e) (get f f_i) `seq` vs

-- |This function determines whether @e@ is an arithmetic
--  expression, or a boolean expression. It also throws an error if the
--  expression is not valid (i.e. a boolean variable is used in an arithmetic
--  expression).
exprType :: Types -> Expression -> Type
exprType vs@(a, v, f, fa) e = case e of
  (TRUE     ) -> BOOL
  (FALSE    ) -> BOOL
  (Const   i) -> INT
  (Func (FuncCall i es)) -> checkFuncCall vs (FuncCall i es) `seq` get f i
  (EVar (Var      i   )) ->                                        get v i
  (EVar (Arr      i ex)) -> checkType (exprType vs ex) INT   `seq` get a i
  (Add e1 e2) ->  checkType (exprType vs e2) . checkType (exprType vs e1) $ INT
  (Sub e1 e2) ->  checkType (exprType vs e2) . checkType (exprType vs e1) $ INT
  (Mul e1 e2) ->  checkType (exprType vs e2) . checkType (exprType vs e1) $ INT
  (Div e1 e2) ->  checkType (exprType vs e2) . checkType (exprType vs e1) $ INT
  (Eq  e1 e2) -> (checkType (exprType vs e2) . checkType (exprType vs e1) $ INT) `seq` BOOL
  (Neq e1 e2) -> (checkType (exprType vs e2) . checkType (exprType vs e1) $ INT) `seq` BOOL
  (Lt  e1 e2) -> (checkType (exprType vs e2) . checkType (exprType vs e1) $ INT) `seq` BOOL
  (Gt  e1 e2) -> (checkType (exprType vs e2) . checkType (exprType vs e1) $ INT) `seq` BOOL
  (Lte e1 e2) -> (checkType (exprType vs e2) . checkType (exprType vs e1) $ INT) `seq` BOOL
  (Gte e1 e2) -> (checkType (exprType vs e2) . checkType (exprType vs e1) $ INT) `seq` BOOL
  (Neg ex   ) ->  checkType (exprType vs ex) BOOL
  (And e1 e2) ->  checkType (exprType vs e2) . checkType (exprType vs e1) $ BOOL
  (Or  e1 e2) ->  checkType (exprType vs e2) . checkType (exprType vs e1) $ BOOL

-- |This function returns the value associated with the association @s@ in the
--  association list @l@. Calls 'error' if @s@ is not in @l@.
get :: [ (String, a) ] -> String -> a
get l s = case lookup s l of
  (Just  t) -> t
  (Nothing) -> error $  "The specified association list doesn't contain "
                     ++ show s
                     ++ "."

-- |This function checks that the arguments to the specified function are of the
--  correct type and there are the correct number of them. Calls 'error' if
--  either of these conditions are not met.
checkFuncCall :: Types -> FuncCall -> [ Type ]
checkFuncCall vs@(a, v, f, fa) (FuncCall i es) = checkType (length es) (length as) `seq` ts
  where as = get fa i
        ts = checkArg <$!> (zip es as)
        checkArg (EVar (Var i'), Arg t i True ) = checkType (get a i') t
        checkArg (ex           , Arg t i False) = checkType (exprType vs ex) t


-- |This function calls 'error' if @a@ is not equal to @b@.
checkType :: (Eq a, Show a) => a -> a -> a
checkType a b = if a == b
  then a
  else error $  show a
             ++ " is not equal to "
             ++ show b
             ++ "."
