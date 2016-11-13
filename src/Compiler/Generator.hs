-- |This module generates the code from the syntax tree. It assumes there is
--  an infinite number of registers available.

module Compiler.Generator
  ( generate
  ) where

import Compiler.SyntaxTree
import Compiler.Instruction
import Compiler.GeneratorState



generate :: Program -> [ Instruction ]
generate (main:funcs) = getInstructions
                      . appendInstructions [ LABEL "_end"
                                           , HALT ]
                      . foldl (flip genFunction) s' $ funcs
  where s  = foldl (flip addFunction) newGeneratorState funcs
        s' = genMain main s
generate _            = error "`main` function not declared."

genMain :: Function -> GeneratorState -> GeneratorState
genMain (Function _ i _ stmts) s = appendInstructions [ LDC r (Right "_end")
                                                      , JMP r ] s'5
  where s'1 = declareVariable (Var "_return") s
        s'2 = declareVariable (Var "_return_addr") s'1
        s'3 = appendInstructions [ LABEL i ] s'2
        s'4 = genStatements stmts s'3
        ([ r ], s'5) = getNRegisters 1 s'4

genFunction :: Function -> GeneratorState -> GeneratorState
genFunction (Function _ i args stmts) s = appendInstructions [ LDC r (Left 1)
                                                             , ADD r r sp
                                                             , LDM r r
                                                             , JMP r ] s'6
  where s'1 = declareVariable (Var "_return") s
        s'2 = declareVariable (Var "_return_addr") s'1
        s'3 = foldl (\s arg -> declareArgument arg s) s'2 args
        s'4 = appendInstructions [ LABEL i ] s'3
        s'5 = genStatements stmts s'4
        ([ r ], s'6) = getNRegisters 1 s'5

genStatements :: [ Statement ] -> GeneratorState -> GeneratorState
genStatements stmts s = resetRegisterNo . foldl (flip genStatement) s' $ stmts
  where s' = resetRegisterNo s

genStatement :: Statement -> GeneratorState -> GeneratorState
genStatement (Declaration _ v) s = declareVariable v s

genStatement (Assignment (Var i) e) s = appendInstructions insts s'3
  where (r1, s'1)     = genExpr e s
        ([ r2 ], s'2) = getNRegisters 1 s'1
        s'3           = case getOffset (Var i) s'2 of
                          (Left  offset ) -> appendInstructions [ LDC r2 (Left offset)
                                                                , ADD r2 r2 sp ] s'2
                          (Right pointer) -> appendInstructions [ LDC r2 (Left pointer)
                                                                , ADD r2 r2 sp
                                                                , LDM r2 r2 ] s'2
        insts         = [ STM r2 r1 ]
genStatement (Assignment (Arr i index) e) s = appendInstructions insts s'4
  where (r1, s'1)     = genExpr e s
        (r2, s'2)     = genExpr index s'1
        ([ r3 ], s'3) = getNRegisters 1 s'2
        s'4           = case getOffset (Arr i index) s'3 of
                          (Left  offset ) -> appendInstructions [ LDC r3 (Left offset)
                                                                , ADD r3 r3 sp ] s'3
                          (Right pointer) -> appendInstructions [ LDC r3 (Left pointer)
                                                                , ADD r3 r3 sp
                                                                , LDM r3 r3 ] s'3
        insts         = [ ADD r3 r3 r2
                        , STM r3 r1
                        ]

genStatement (Cond e s1 s2) s = appendInstructions [ LABEL l2 ] s'6
  where (r1, s'1)         = genExpr e s
        ([ l1, l2 ], s'2) = getNLabels 2 s'1
        s'3               = appendInstructions [ BEZ r1 (Right l1) ] s'2
        s'4               = genStatements s1 s'3
        s'5               = appendInstructions [ LDC r1 (Right l2)
                                               , JMP r1
                                               , LABEL l1 ] s'4
        s'6               = genStatements s2 s'5

genStatement (While e st) s = appendInstructions insts s'5
  where ([ l1, l2 ], s'1) = getNLabels 2 s
        s'2               = appendInstructions [ LABEL l1 ] s'1
        (r1, s'3)         = genExpr e s'2
        s'4               = appendInstructions [ BEZ r1 (Right l2) ] s'3
        s'5               = genStatements st s'4
        insts             = [ LDC r1 (Right l1)
                            , JMP r1
                            , LABEL l2
                            ]

genStatement (FunctionCall f) s = snd . genFuncCall f $ s

genStatement (Return e) s = appendInstructions insts s'2
  where (r1, s'1)                 = genExpr e s
        ([ r2 ], s'2)             = getNRegisters 1 s'1
        (Left return_offset)      = getOffset (Var "_return") s'2
        (Left return_addr_offset) = getOffset (Var "_return_addr") s'2
        insts                     = [ LDC r2 (Left return_offset)
                                    , ADD r2 r2 sp
                                    , STM r2 r1
                                    , LDC r2 (Left return_addr_offset)
                                    , ADD r2 r2 sp
                                    , LDM r2 r2
                                    , JMP r2
                                    ]

genFuncCall :: FuncCall -> GeneratorState -> (Int, GeneratorState)
genFuncCall (FuncCall i exprs) s = (r1, s'5)
  where args                  = getFunctionArgs i s
        params                = zip args exprs
        stack_size            = getStackSize s
        ([ l ], s'1)          = getNLabels 1 s
        ([ r1, r2, r3 ], s'2) = getNRegisters 3 s'1
        s'3                   = appendInstructions [ LDC r1 (Left stack_size)
                                                   , ADD sp sp r1
                                                   , LDC r2 (Left 1)
                                                   , ADD r3 r2 sp
                                                   , LDC r1 (Right l)
                                                   , STM r3 r1 ] s'2
        s'4                   = foldl pushParam s'3 params
        s'5                   = appendInstructions [ LDC r2 (Right i)
                                                   , JMP r2
                                                   , LABEL l
                                                   , LDM r1 sp -- Load the return value into r1.
                                                   , LDC r2 (Left stack_size)
                                                   , SUB sp sp r2 ] s'4
        pushParam s (Arg _ _ True, EVar v) = appendInstructions [ ADD r3 r3 r2
                                                                , STM r3 r ] s'
          where (r, s') = case getOffset v s of
                            (Left  offset ) -> (r1, appendInstructions [ LDC r1 (Left $ stack_size - offset)
                                                                       , SUB r1 sp r1 ] s)
                            (Right pointer) -> (r1, appendInstructions [ LDC r1 (Left $ stack_size - pointer)
                                                                       , SUB r1 sp r1
                                                                       , LDM r1 r1 ] s)
        pushParam s (_           , ex    ) = appendInstructions [ ADD r3 r3 r2
                                                                , STM r3 r ] s'
          where (r, s') = genExpr ex s

genExpr :: Expression -> GeneratorState -> (Int, GeneratorState)
genExpr (TRUE     ) s = (r, appendInstructions [ LDC r (Left 1) ] s'1)
  where ([ r ], s'1) = getNRegisters 1 s

genExpr (FALSE    ) s = (r, appendInstructions [ LDC r (Left 0) ] s'1)
  where ([ r ], s'1) = getNRegisters 1 s

genExpr (Const   i) s = (r, appendInstructions [ LDC r (Left i) ] s'1)
  where ([ r ], s'1) = getNRegisters 1 s

genExpr (Func   fn) s = genFuncCall fn s

genExpr (EVar    v) s = (r , s'3)
  where ([ r ], s'1) = getNRegisters 1 s
        s'2          = case getOffset v s'1 of
                         (Left  offset ) -> appendInstructions [ LDC r (Left offset)
                                                               , ADD r r sp ] s'1
                         (Right pointer) -> appendInstructions [ LDC r (Left pointer)
                                                               , ADD r r sp
                                                               , LDM r r ] s'1
        s'3          = case v of
                         (Var i      ) -> appendInstructions [ LDM r r ] s'2
                         (Arr i index) -> let (r1, s'4) = genExpr index s'2 in
                                          appendInstructions [ ADD r r r1
                                                             , LDM r r ] s'4

genExpr (Add e1 e2) s = (r, appendInstructions [ ADD r r1 r2 ] s'3)
  where (r1, s'1)    = genExpr e1 s
        (r2, s'2)    = genExpr e2 s'1
        ([ r ], s'3) = getNRegisters 1 s'2

genExpr (Sub e1 e2) s = (r, appendInstructions [ SUB r r1 r2 ] s'3)
  where (r1, s'1)    = genExpr e1 s
        (r2, s'2)    = genExpr e2 s'1
        ([ r ], s'3) = getNRegisters 1 s'2

genExpr (Mul e1 e2) s = (r, appendInstructions [ MUL r r1 r2 ] s'3)
  where (r1, s'1)    = genExpr e1 s
        (r2, s'2)    = genExpr e2 s'1
        ([ r ], s'3) = getNRegisters 1 s'2

genExpr (Div e1 e2) s = (r, appendInstructions [ DIV r r1 r2 ] s'3)
  where (r1, s'1)    = genExpr e1 s
        (r2, s'2)    = genExpr e2 s'1
        ([ r ], s'3) = getNRegisters 1 s'2

genExpr (Eq  e1 e2) s = (r, appendInstructions [ CEQ r r1 r2 ] s'3)
  where (r1, s'1)    = genExpr e1 s
        (r2, s'2)    = genExpr e2 s'1
        ([ r ], s'3) = getNRegisters 1 s'2

genExpr (Neq e1 e2) s = (r, appendInstructions [ CEQ r r1 r2
                                               , NOT r r ] s'3)
  where (r1, s'1)    = genExpr e1 s
        (r2, s'2)    = genExpr e2 s'1
        ([ r ], s'3) = getNRegisters 1 s'2

genExpr (Lt  e1 e2) s = (r, appendInstructions [ CGT r r2 r1 ] s'3)
  where (r1, s'1)    = genExpr e1 s
        (r2, s'2)    = genExpr e2 s'1
        ([ r ], s'3) = getNRegisters 1 s'2

genExpr (Gt  e1 e2) s = (r, appendInstructions [ CGT r r1 r2 ] s'3)
  where (r1, s'1)    = genExpr e1 s
        (r2, s'2)    = genExpr e2 s'1
        ([ r ], s'3) = getNRegisters 1 s'2

genExpr (Lte e1 e2) s = (r, appendInstructions [ CGT r r1 r2
                                               , NOT r r ] s'3)
  where (r1, s'1)    = genExpr e1 s
        (r2, s'2)    = genExpr e2 s'1
        ([ r ], s'3) = getNRegisters 1 s'2

genExpr (Gte e1 e2) s = (r, appendInstructions [ CGT r r2 r1
                                               , NOT r r ] s'3)
  where (r1, s'1)    = genExpr e1 s
        (r2, s'2)    = genExpr e2 s'1
        ([ r ], s'3) = getNRegisters 1 s'2

genExpr (Neg ex   ) s = (r, appendInstructions [ NOT r r1 ] s'2)
  where (r1, s'1)    = genExpr ex s
        ([ r ], s'2) = getNRegisters 1 s'1

genExpr (And e1 e2) s = (r, appendInstructions [ AND r r1 r2 ] s'3)
  where (r1, s'1)    = genExpr e1 s
        (r2, s'2)    = genExpr e2 s'1
        ([ r ], s'3) = getNRegisters 1 s'2

genExpr (Or  e1 e2) s = (r, appendInstructions [ OR  r r1 r2 ] s'3)
  where (r1, s'1)    = genExpr e1 s
        (r2, s'2)    = genExpr e2 s'1
        ([ r ], s'3) = getNRegisters 1 s'2
