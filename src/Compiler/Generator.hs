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
-- |This module generates the code from the syntax tree. It assumes there is
--  an infinite number of registers available.
module Compiler.Generator
  ( generate
  ) where

import Control.Monad.State
import Compiler.GeneratorState

-- |This function generates @prog@, and returns a list of instructions.
generate :: Program -> [ Instruction ]
generate prog = evalState (generate' prog) newGenerator

-- |This functions generates @prog@.
generate' :: Program -> State Generator [ Instruction ]
generate' (main:funcs) = do addInst $ LDC sp (Left 0)
                            mapM addFunction funcs
                            genMain main
                            mapM genFunction funcs
                            addInst $ LABEL "_end"
                            addInst $ HALT
                            insts <- getInsts
                            return insts

-- |This function generates the main function.
genMain :: Function -> State Generator ()
genMain (Function _ i _ stmts) = do declareVariable (Var "_return")
                                    declareVariable (Var "_return_addr")
                                    addInst $ LABEL i
                                    genStatements stmts
                                    setRegister gprb
                                    r <- getRegister
                                    addInst $ LDC r (Right "_end")
                                    addInst $ JMP r
                                    resetStack

-- |This function generates a function.
genFunction :: Function -> State Generator ()
genFunction (Function _ i args stmts) = do declareVariable (Var "_return")
                                           declareVariable (Var "_return_addr")
                                           mapM declareArgument args
                                           addInst $ LABEL i
                                           genStatements stmts
                                           setRegister gprb
                                           r <- getRegister
                                           addInst $ LDC r (Left 4)
                                           addInst $ ADD r r sp
                                           addInst $ LDM r r
                                           addInst $ JMP r
                                           resetStack

-- |This function generates the statements @stmts@.
genStatements :: [ Statement ] -> State Generator ()
genStatements stmts = mapM_ (\stmt -> do setRegister gprb
                                         genStatement stmt) stmts

-- |This function generates a statement.
genStatement :: Statement -> State Generator ()
genStatement (Declaration _ v)
  = declareVariable v

genStatement (Assignment (Var i) e)
  = do r1 <- genExpr e
       r2 <- getRegister
       offset <- getOffset (Var i)
       case offset of
         (Left  o) -> do addInst $ LDC r2 (Left o)
                         addInst $ ADD r2 r2 sp
                         addInst $ STM r2 r1
         (Right p) -> do addInst $ LDC r2 (Left p)
                         addInst $ ADD r2 r2 sp
                         addInst $ LDM r2 r2
                         addInst $ STM r2 r1

genStatement (Assignment (Arr i index) e)
  = do r1 <- genExpr e
       r2 <- genExpr (Mul (Const 4) index)
       r3 <- getRegister
       offset <- getOffset (Arr i index)
       case offset of
         (Left  o) -> do addInst $ LDC r3 (Left o)
                         addInst $ ADD r3 r3 sp
                         addInst $ ADD r3 r3 r2
                         addInst $ STM r3 r1
         (Right p) -> do addInst $ LDC r3 (Left p)
                         addInst $ ADD r3 r3 sp
                         addInst $ LDM r3 r3
                         addInst $ ADD r3 r3 r2
                         addInst $ STM r3 r1

genStatement (Cond e s1 s2)
  = do r1 <- genExpr e
       elseL <- getLabel
       endL <- getLabel
       addInst $ BEZ r1 (Right elseL)
       genStatements s1
       addInst $ LDC r1 (Right endL)
       addInst $ JMP r1
       addInst $ LABEL elseL
       genStatements s2
       addInst $ LABEL endL

genStatement (While e st)
  = do startL <- getLabel
       endL <- getLabel
       addInst $ LABEL startL
       r1 <- genExpr e
       addInst $ BEZ r1 (Right endL)
       genStatements st
       addInst $ LDC r1 (Right startL)
       addInst $ JMP r1
       addInst $ LABEL endL

genStatement (FunctionCall f)
  = do genFuncCall f
       return ()

genStatement (Return e)
  = do r1 <- genExpr e
       r2 <- getRegister
       (Left ret_o) <- getOffset (Var "_return")
       (Left ret_addr_o) <- getOffset (Var "_return_addr")
       addInst $ LDC r2 (Left ret_o)
       addInst $ ADD r2 r2 sp
       addInst $ STM r2 r1
       addInst $ LDC r2 (Left ret_addr_o)
       addInst $ ADD r2 r2 sp
       addInst $ LDM r2 r2
       addInst $ JMP r2

-- |This function generates a function call statement.
genFuncCall :: FuncCall -> State Generator Int
genFuncCall (FuncCall i exprs)
  = do args <- getFunctionArgs i
       stack_size <- getStackSize
       retL <- getLabel
       r1 <- getRegister
       r2 <- getRegister
       r3 <- getRegister
       addInst $ LDC r1 (Left stack_size)
       addInst $ ADD sp sp r1
       addInst $ LDC r2 (Left 4)
       addInst $ ADD r3 r2 sp
       addInst $ LDC r1 (Right retL)
       addInst $ STM r3 r1
       mapM (pushParam r1 r2 r3 stack_size) (zip args exprs)
       addInst $ LDC r2 (Right i)
       addInst $ JMP r2
       addInst $ LABEL retL
       addInst $ LDM r1 sp
       addInst $ LDC r2 (Left stack_size)
       addInst $ SUB sp sp r2
       return r1
  where pushParam r1 r2 r3 stack_size (Arg _ _ True, EVar v)
          = do offset <- getOffset v
               case offset of
                 (Left  o) -> do addInst $ LDC r1 (Left $ stack_size - o)
                                 addInst $ SUB r1 sp r1
                                 addInst $ ADD r3 r3 r2
                                 addInst $ STM r3 r1
                 (Right p) -> do addInst $ LDC r1 (Left $ stack_size - p)
                                 addInst $ SUB r1 sp r1
                                 addInst $ LDM r1 r1
                                 addInst $ ADD r3 r3 r2
                                 addInst $ STM r3 r1
        pushParam r1 r2 r3 stack_size (_           , ex    )
          = do r <- genExpr ex
               addInst $ ADD r3 r3 r2
               addInst $ STM r3 r

-- |This function generates an expression. It returns the register containing
--  the result of the expression.
genExpr :: Expression -> State Generator Int
genExpr (TRUE     )
  = do r <- getRegister
       addInst $ LDC r (Left 1)
       return r

genExpr (FALSE    )
  = do r <- getRegister
       addInst $ LDC r (Left 0)
       return r

genExpr (Const   i)
  = do r <- getRegister
       addInst $ LDC r (Left i)
       return r

genExpr (Func   fn)
  = genFuncCall fn

genExpr (EVar    v)
  = do r <- getRegister
       offset <- getOffset v
       case offset of
         (Left  o) -> do addInst $ LDC r (Left o)
                         addInst $ ADD r r sp
                         case v of
                           (Var i      ) -> do addInst $ LDM r r
                                               return r
                           (Arr i index) -> do r1 <- genExpr index
                                               addInst $ ADD r r r1
                                               addInst $ LDM r r
                                               return r
         (Right p) -> do addInst $ LDC r (Left p)
                         addInst $ ADD r r sp
                         addInst $ LDM r r
                         case v of
                           (Var i      ) -> do addInst $ LDM r r
                                               return r
                           (Arr i index) -> do r1 <- genExpr index
                                               addInst $ ADD r r r1
                                               addInst $ LDM r r
                                               return r

genExpr (Add e1 e2)
  = do r1 <- genExpr e1
       r2 <- genExpr e2
       addInst $ ADD r1 r1 r2
       return r1

genExpr (Sub e1 e2)
  = do r1 <- genExpr e1
       r2 <- genExpr e2
       addInst $ SUB r1 r1 r2
       return r1

genExpr (Mul e1 e2)
  = do r1 <- genExpr e1
       r2 <- genExpr e2
       addInst $ MUL r1 r1 r2
       return r1

genExpr (Div e1 e2)
  = do r1 <- genExpr e1
       r2 <- genExpr e2
       addInst $ DIV r1 r1 r2
       return r1

genExpr (Eq  e1 e2)
  = do r1 <- genExpr e1
       r2 <- genExpr e2
       addInst $ CEQ r1 r1 r2
       return r1

genExpr (Neq e1 e2)
  = do r1 <- genExpr e1
       r2 <- genExpr e2
       addInst $ CEQ r1 r1 r2
       addInst $ NOT r1 r1
       addInst $ LDC r2 (Left 1)
       addInst $ AND r1 r1 r2
       return r1

genExpr (Lt  e1 e2)
  = do r1 <- genExpr e1
       r2 <- genExpr e2
       addInst $ CGT r1 r2 r1
       return r1

genExpr (Gt  e1 e2)
  = do r1 <- genExpr e1
       r2 <- genExpr e2
       addInst $ CGT r1 r1 r2
       return r1

genExpr (Lte e1 e2)
  = do r1 <- genExpr e1
       r2 <- genExpr e2
       addInst $ CGT r1 r1 r2
       addInst $ NOT r1 r1
       addInst $ LDC r2 (Left 1)
       addInst $ AND r1 r1 r2
       return r1

genExpr (Gte e1 e2)
  = do r1 <- genExpr e1
       r2 <- genExpr e2
       addInst $ CGT r1 r2 r1
       addInst $ NOT r1 r1
       addInst $ LDC r2 (Left 1)
       addInst $ AND r1 r1 r2
       return r1

genExpr (Neg ex   )
  = do r1 <- genExpr ex
       r2 <- getRegister
       addInst $ NOT r1 r1
       addInst $ LDC r2 (Left 1)
       addInst $ AND r1 r1 r2
       return r1

genExpr (And e1 e2)
  = do r1 <- genExpr e1
       r2 <- genExpr e2
       addInst $ AND r1 r1 r2
       return r1

genExpr (Or  e1 e2)
  = do r1 <- genExpr e1
       r2 <- genExpr e2
       addInst $ OR r1 r1 r2
       return r1
