module Compiler.Analyse
  ( analyse
  ) where

import Compiler.Types
import Compiler.SyntaxTree

import Data.Maybe
import qualified Data.Map.Strict as Map

type Vars = (FunctionMap, VariableMap)

-- TODO: Make this code nicer, check there is a main function and that is has
-- no arguments. Check that there is at least on return statement on every path throught the function.

analyse :: Program -> Program
analyse prog = snd . foldr (\func (vs, f) -> let (vs', func') = analyseFunction func vs in (vs', f ++ [func'])) (vs, []) $ prog
  where f = foldr (\(Function t i args _) f -> Map.insertWith (err $ "Function " ++ show i ++ " declared twice.") i (t, map (\(Arg t _) -> t) args, map (\(Arg _ i) -> i) args) f) Map.empty prog
        vs = (f, Map.empty)

analyseFunction :: Function -> Vars -> (Vars, Function)
analyseFunction (Function t i args stmts) (f, v) = ((f', v'), Function t i args stmts')
  where ((f', v'), stmts') = analyseStatements stmts i (f, v)

analyseStatements :: [ Statement ] -> Identifier -> Vars -> (Vars, [ Statement ])
analyseStatements stmts i vs = foldr (\stmt (vs, s) -> let (vs', stmt') = analyseStatement stmt i vs in (vs', s ++ [stmt'])) (vs, []) stmts

analyseStatement :: Statement -> Identifier -> Vars -> (Vars, Statement)
analyseStatement s f vs = case s of
  (Declaration d)     -> let (vs', d') = analyseDecVar d vs in (vs', Declaration d')
  (Assignment  a)     -> let (vs', a') = analyseAssign a vs in (vs', Assignment  a')
  (AssignDeclr a)     -> let (vs', a') = analyseAssignDecl a vs in (vs', AssignDeclr a')
  (Cond e s1 s2)      -> (vs, Cond (checkBool e vs) (snd . analyseStatements s1 f $ vs) (snd . analyseStatements s2 f $ vs))
  (While e s)         -> (vs, While (checkBool e vs) (snd . analyseStatements s f $ vs))
  (For mad mex mas s) -> let (vs', mad') = if isJust mad
                                             then mapSnd Just . analyseAssignDecl (fromJust mad) $ vs
                                             else (vs, Nothing)
                             mex' = Just $ checkBool (fromMaybe TRUE mex) vs'
                             (vs'', mas') = if isJust mas
                                               then mapSnd Just . analyseAssign (fromJust mas) $ vs'
                                               else (vs', Nothing)
                             in (vs'', For mad' mex' mas' (snd . analyseStatements s f $ vs''))
  (FunctionCall f)    -> let (vs', f') = analyseFuncCall f vs in (vs', FunctionCall f')
  (Return ex)         -> if extractFType (fst vs) f == expressionType vs ex
                           then (vs, Return ex)
                           else error "Return expression is not of the same type as function."
  where checkBool ex vars = checkType vars ex BOOL "Expression is not of type BOOL."
        mapSnd f (a, b) = (a, f b)

analyseDecVar :: DecVar -> Vars -> (Vars, DecVar)
analyseDecVar (DecVar t i mex) (f, v) = ((f, v'), DecVar t i (Just (Const size)))
  where size = evalAExp $ fromMaybe (Const 1) mex
        v'   = Map.insertWith (err $ "Variable " ++ show i ++ " declared twice.") i (t, 0) v

analyseAssign :: Assign -> Vars -> (Vars, Assign)
analyseAssign (Assign av@(AssVar i _) ex) vs@(f, v) = (vs, Assign av' ex')
  where av' = snd . analyseAssVar av $ vs
        ex' = checkType vs ex (extractVType v i) "Type of expression doesn't match that of the variable it is assigned to."

analyseAssVar :: AssVar -> Vars -> (Vars, AssVar)
analyseAssVar (AssVar i mex) (f, v) = ((f, v), AssVar i' (Just mex'))
  where i'   = if Map.member i v then i else error $ "Variable " ++ show i ++ " not declared before it is used."
        mex' = checkType (f, v) (fromMaybe (Const 0) mex) INT "Array index is not of type INT."

analyseAssignDecl :: AssignDecl -> Vars -> (Vars, AssignDecl)
analyseAssignDecl (AssignDecl d@(DecVar t _ _) ex) (f, v) = ((f, v'), AssignDecl d' ex')
  where ((_, v'), d') = analyseDecVar d (f, v)
        ex' = checkType (f, v) ex t "Type of expression doesn't match that of the variable it is assigned to."

analyseFuncCall :: FuncCall -> Vars -> (Vars, FuncCall)
analyseFuncCall (FuncCall i args) (f, v) = ((f, v), FuncCall i' args')
  where i'    = if Map.member i f
                  then i
                  else error $ "Function " ++ show i ++ " is used but not declared."
        args' = if extractFArgTypes f i == map (expressionType (f, v)) args
                  then args
                  else error $ "Types of arguments to function " ++ show i ++ " don't match. Expected: " ++ show (extractFArgTypes f i) ++ ". Found: " ++ show (map (expressionType (f, v)) args) ++ "."

-- |This function calls 'error' with @message@ as the error message.
err :: String -> a -> a -> b
err message _ _ = error message

-- |This function checks that @e@ is of type @t@. If @e@ is not of type @t@,
--  'error' is called with @message@.
checkType :: Vars -> Expression -> Type -> String -> Expression
checkType vs e t message = if expressionType vs e == t
                              then e
                              else error message

-- |This function evaluates an expression. If the expression contains variables,
--  function calls or any boolean logic, 'error' is called.
evalAExp :: Expression -> Int
evalAExp (Const i  ) = i
evalAExp (Add e1 e2) = evalAExp e1 + evalAExp e2
evalAExp (Sub e1 e2) = evalAExp e1 - evalAExp e2
evalAExp (Mul e1 e2) = evalAExp e1 * evalAExp e2
evalAExp (Div e1 e2) = evalAExp e1 `div` evalAExp e2
evalAExp _           = error "Variables and function calls are not allowed in array declaration."

-- |This function determines whether @e@ is an arithmetic
--  expression, or a boolean expression. It also throws an error if the
--  expression is not valid (i.e. a boolean variable is used in an arithmetic
--  expression).
expressionType :: Vars -> Expression -> Type
expressionType vs@(f, v) e = case e of
  (TRUE   ) -> BOOL
  (FALSE  ) -> BOOL
  (Const i) -> INT
  (Func (FuncCall i es)) -> if extractFArgTypes f i == map (expressionType vs) es
                              then extractFType f i
                              else error $ "Types of arguments to function "
                                           ++ show i
                                           ++ " don't match. Expected: "
                                           ++ show (extractFArgTypes f i)
                                           ++ ". Found: "
                                           ++ show (map (expressionType vs) es)
                                           ++ "."
  (Var  (AssVar   i ex)) -> if isNothing ex || expressionType vs (fromJust ex) == INT
                              then extractVType v i
                              else error $ "Array index expression was not of type INT."
  (Add e1 e2) -> checkBinTypes vs e1 e2 "Add" INT INT
  (Sub e1 e2) -> checkBinTypes vs e1 e2 "Sub" INT INT
  (Mul e1 e2) -> checkBinTypes vs e1 e2 "Mul" INT INT
  (Div e1 e2) -> checkBinTypes vs e1 e2 "Div" INT INT
  (Eq  e1 e2) -> if expressionType vs e1 == expressionType vs e2
                   then BOOL
                   else error $ "Type of operands to Eq do not match."
  (Lt  e1 e2) -> checkBinTypes vs e1 e2 "Lt"  INT BOOL
  (Gt  e1 e2) -> checkBinTypes vs e1 e2 "Gt"  INT BOOL
  (Lte e1 e2) -> checkBinTypes vs e1 e2 "Lte" INT BOOL
  (Gte e1 e2) -> checkBinTypes vs e1 e2 "Gte" INT BOOL
  (Neg ex   ) -> if expressionType vs ex == BOOL
                   then BOOL
                   else error $ "Operand to Neg is not of type BOOL."
  (And e1 e2) -> checkBinTypes vs e1 e2 "And" BOOL BOOL
  (Or  e1 e2) -> checkBinTypes vs e1 e2 "Or"  BOOL BOOL

-- |This function checks that the @e1@ and @e2@ have the same type, @check_t@.
--  If they do, then @return_t@ is returned, otherwise 'error' is called,
--  using @operator@ to describe where the problem is.
checkBinTypes :: Vars -> Expression  -> Expression  -> String -> Type -> Type -> Type
checkBinTypes vs e1 e2 operator check_t return_t =
  if expressionType vs e1 == check_t && expressionType vs e2 == check_t
    then return_t
    else error $ "Operand(s) to " ++ operator ++ " are not of type " ++ show check_t ++ "."

-- |This function returns the type associated with @i@. If @i@ is not in the
--  FunctionMap @f@, 'error' is called.
extractFType :: FunctionMap -> Identifier -> Type
extractFType f i = case Map.lookup i f of
  (Just (t, _, _)) -> t
  (Nothing       ) -> error $ "Function " ++ show i ++ " used but not declared."

-- |This function returns the list of types associated with the arguments of
--  @i@. If @i@ is not in the FunctionMap @f@, 'error' is called.
extractFArgTypes :: FunctionMap -> Identifier -> [ Type ]
extractFArgTypes f i = case Map.lookup i f of
  (Just (_, ts, _)) -> ts
  (Nothing        ) -> error $ "Function " ++ show i ++ " used but not declared."

-- |This function returns the type associated with @i@. If @i@ is not in the
--  VariableMap @v@, 'error' is called.
extractVType :: VariableMap -> Identifier -> Type
extractVType v i = case Map.lookup i v of
  (Just (t, _)) -> t
  (Nothing    ) -> error $ "Variable " ++ show i ++ " used but not declared."

-- |This function returns the Address associated with @i@. If @i@ is not in the
--  VariableMap @v@, 'error' is called.
extractVAddress :: VariableMap -> Identifier -> Address
extractVAddress v i = case Map.lookup i v of
  (Just (_, a)) -> a
  (Nothing    ) -> error $ "Variable " ++ show i ++ " used but not declared."
