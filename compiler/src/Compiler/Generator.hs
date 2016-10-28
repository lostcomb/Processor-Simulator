-- |This module generates the code from the syntax tree. It assumes there is
--  an infinite number of registers available.

module Compiler.Generator
  ( generate
  ) where

import Data.Maybe (listToMaybe)
import Compiler.SyntaxTree
import Compiler.Instruction

type Size     = Int
type SPOffset = Int
type LabelNo  = Int
type Vars     = [ (Identifier, Register        ) ]
type Stack    = [ (Identifier, (SPOffset, Size)) ]

pc, sp :: Register
pc = 0
sp = 1

generate :: Program -> [ Instruction ]
generate (main:funcs) = [ LDC sp (Left 0)
                        , LDC (sp + 1) (Right "main")
                        , JMP (sp + 1) ] ++
                        is ++
                        [ LABEL "_end" ]
  where (_, is) = foldl (\(l, is) f -> let (l', is') = genFunction l f
                                       in  (l', is ++ is')) (genMain 0 main) funcs
generate _            = error "`main` function not declared."

genMain :: LabelNo -> Function -> (LabelNo, [ Instruction ])
genMain l (Function _ i _ stmts) = ( l'
                                   , [ LABEL i ] ++
                                     is ++
                                     [ LDC (sp + 1) (Right "_end")
                                     , JMP (sp + 1) ])
  where st          = [ ("_return_addr", (1, 1)), ("_return", (0, 1)) ]
        (l', _, is) = genStatements [] st stmts l

genFunction :: LabelNo -> Function -> (LabelNo, [ Instruction ])
genFunction l (Function t i args stmts) = ( l'
                                          , [ LABEL i ] ++
                                            is ++
                                            is' ++
                                            [ JMP (r - 1) ])
  where st            =  (reverse . zip (map (\(Arg _ i _) -> i) args)
                                  $ [(x, 1) | x <- [2..length args + 2]])
                      ++ [ ("_return_addr", (1, 1)), ("_return", (0, 1)) ]
        (l', vs, is ) = genStatements [] st stmts l
        (r , _ , is') = getVar vs st (Var "_return_addr") (sp + 1)

genStatements :: Vars -> Stack -> [ Statement ] -> LabelNo
              -> (LabelNo, Vars, [ Instruction ])
genStatements vs st stmts l = (l', vs', is)
  where (l', vs', st', is) = foldl genS (l, vs, st, []) stmts
        genS (l, vs, st, is) s = (_l, _vs, _st, is ++ _is)
          where (_, _l, _vs, _st, _is) = genStatement vs st s (sp + 1) l

-- |This function generates the instructions and associated state which i
--  required to generate the instruction for the specified statement.
genStatement :: Vars -> Stack -> Statement -> Register -> LabelNo
             -> (Register, LabelNo, Vars, Stack, [ Instruction ])
genStatement vs st (Declaration t v) r l = (r, l, vs, (i, (off, s)):st, [])
  where (i, s) = case v of
                   (Var i          ) -> (i, 1)
                   (Arr i (Const s)) -> (i, s)
        off    = case listToMaybe st of
                   (Just (_, (off, size))) -> off + size
                   (Nothing              ) -> 0

genStatement vs st (Assignment  v e) r l = (r', l, vs', st, is)
  where (r', vs', is) = setVar vs st v e r

genStatement vs st (Cond e s1 s2) r l = ( r' + 1
                                        , l' + 2
                                        , vs'
                                        , st
                                        , _is ++
                                          [ BEZ (Right else_l) (r' - 1) ] ++
                                          is' ++
                                          [ LDC r' (Right end_l)
                                          , JMP r'
                                          , LABEL else_l ] ++
                                          is'' ++
                                          [ LABEL end_l ])
  where ( r',  _vs , _is  ) = genExpr vs st e r
        (_l , __vs ,  is' ) = genStatements _vs st s1 l
        ( l',   vs',  is'') = genStatements __vs st s2 _l
        else_l = show l'
        end_l  = show (l' + 1)

genStatement vs st (While e s) r l = ( r' + 1
                                     , l' + 2
                                     , vs'
                                     , st
                                     , [ LABEL start_l ] ++
                                       is ++
                                       [ BEZ (Right end_l) (r' - 1) ] ++
                                       is' ++
                                       [ LDC r' (Right start_l)
                                       , JMP r'
                                       , LABEL end_l ])
  where (r', _vs , is ) = genExpr vs st e r
        (l',  vs', is') = genStatements _vs st s l
        start_l = show l'
        end_l   = show (l' + 1)

genStatement vs st (FunctionCall f) r l = (r', l, vs', st, is)
  where (r', vs', is) = genFuncCall vs st f r

genStatement vs st (Return e) r l = (r', l, vs', st, is ++
                                                     is' ++
                                                     [ JMP (r' - 1) ])
  where (_r , _vs , is ) = setVar vs st (Var "_return") e r
        ( r',  vs', is') = getVar _vs st (Var "_return_addr") _r

genFuncCall :: Vars -> Stack -> FuncCall -> Register -> (Register, Vars, [ Instruction ])
genFuncCall = undefined

-- |This function generates the instructions to evaluate the expression @e@.
genExpr :: Vars -> Stack -> Expression -> Register -> (Register, Vars, [ Instruction ])
genExpr vs st e r = case e of
  (TRUE     ) -> (r + 1, vs, [ LDC r (Left 1) ])
  (FALSE    ) -> (r + 1, vs, [ LDC r (Left 0) ])
  (Const   i) -> (r + 1, vs, [ LDC r (Left $ fromIntegral i) ])
  (Func   fn) -> genFuncCall vs st fn r
  (EVar    v) -> getVar vs st v r
  (Add e1 e2) -> bin r e1 e2 ADD
  (Sub e1 e2) -> bin r e1 e2 SUB
  (Mul e1 e2) -> bin r e1 e2 MUL
  (Div e1 e2) -> bin r e1 e2 DIV
  (Eq  e1 e2) -> bin r e1 e2 CEQ
  (Neq e1 e2) -> genExpr vs st (Neg (Eq e1 e2)) r
  (Lt  e1 e2) -> bin r e2 e1 CGT
  (Gt  e1 e2) -> bin r e1 e2 CGT
  (Lte e1 e2) -> genExpr vs st (Neg (Gt e1 e2)) r
  (Gte e1 e2) -> genExpr vs st (Neg (Lt e1 e2)) r
  (Neg ex   ) -> uni r ex    NOT
  (And e1 e2) -> bin r e1 e2 AND
  (Or  e1 e2) -> bin r e1 e2 OR
  where uni r ex    cons = (r'  + 1, vs' , ex_i ++ [ cons r' (r' - 1) ])
          where (r' , vs' , ex_i) = genExpr vs  st ex r
        bin r e1 e2 cons = (r'' + 1, vs'', e1_i ++ e2_i ++ [ cons r'' (r' - 1) (r'' - 1) ])
          where (r' , vs' , e1_i) = genExpr vs  st e1 r
                (r'', vs'', e2_i) = genExpr vs' st e2 r'

-- |This function retrieves the value of the specified variable from the stack.
--  It first looks in the Vars list to see if the memory location has already
--  been calculated. If the variable is not in this list, it then calculates the
--  memory location using the stack information.
getVar :: Vars -> Stack -> Variable -> Register -> (Register, Vars, [ Instruction ])
getVar vs st (Var i) r = case lookup i vs of
  (Just reg) -> (r + 1, vs, [ LDM r reg ])
  (Nothing ) -> case lookup i st of
    (Just (off, _)) -> (r + 3, (i, r + 1):vs, [ LDC r (Left $ fromIntegral off)
                                              , ADD (r + 1) r sp
                                              , LDM (r + 2) (r + 1)
                                              ])
    (Nothing      ) -> error $  "Variable "
                             ++ show i
                             ++ " has not been declared."

getVar vs st (Arr i ex) r = case lookup i vs' of
  (Just reg) -> (r' + 2, vs, is ++ [ ADD r' reg (r' - 1)
                                   , LDM (r' + 1) r'
                                   ])
  (Nothing ) -> case lookup i st of
    (Just (off, _)) -> (r' + 4, (i, r' + 1):vs, is ++ [ LDC r' (Left $ fromIntegral off)
                                                      , ADD (r' + 1) r' sp
                                                      , ADD (r' + 2) (r' + 1) (r' - 1)
                                                      , LDM (r' + 3) (r' + 2)
                                                      ])
    (Nothing      ) -> error $  "Array "
                             ++ show i
                             ++ " has not been declared."
  where (r', vs', is) = genExpr vs st ex r

-- |This function sets the value of the specified variable to that of the
--  specified expression. It first looks in the Vars list to see if the memory
--  location has already been calculated. If the variable is not in this list,
--  it then calculates the memory location using the stack information.
setVar :: Vars -> Stack -> Variable -> Expression -> Register -> (Register, Vars, [ Instruction ])
setVar vs st (Var i) e r = case lookup i vs of
  (Just reg) -> (r', vs', is ++ [ STM reg (r' - 1) ])
  (Nothing ) -> case lookup i st of
    (Just (off, _)) -> (r' + 2, (i, r' + 1):vs, is ++ [ LDC r' (Left $ fromIntegral off)
                                                      , ADD (r' + 1) r' sp
                                                      , STM (r' + 1) (r' - 1)
                                                      ])
    (Nothing      ) -> error $  "Variable "
                             ++ show i
                             ++ " has not been declared."
  where (r', vs', is) = genExpr vs st e r

setVar vs st (Arr i ex) e r = case lookup i vs of
  (Just reg) -> (r' + 1, vs', is ++ is' ++ [ ADD r' reg (_r - 1)
                                           , STM r' (r' - 1)
                                           ])
  (Nothing ) -> case lookup i st of
    (Just (off, _)) -> (r' + 3, (i, r' + 1):vs', is ++ is' ++ [ LDC r' (Left $ fromIntegral off)
                                                              , ADD (r' + 1) r' sp
                                                              , ADD (r' + 2) (r' + 1) (_r - 1)
                                                              , STM (r' + 2) (r' - 1)
                                                              ])
    (Nothing      ) -> error $  "Array "
                             ++ show i
                             ++ " has not been declared."
  where (_r, _vs, is ) = genExpr  vs st ex r
        (r', vs', is') = genExpr _vs st e _r
