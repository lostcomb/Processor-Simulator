-- |This module generates the code from the syntax tree. It assumes there is
--  an infinite number of registers available.

module Compiler.Generator
  ( generate
  ) where

import Compiler.Types
import Compiler.Analyse
import Compiler.SyntaxTree
import Assembly.Instruction

import Data.Maybe
import qualified Data.Map.Strict as Map

data StackFrame   = StackFrame   { vars         :: VariableMap
                                 , nextFramePos :: SPOffset
                                 , push         :: DecVar -> ProgramState -> ProgramState
                                 , assign       :: Assign -> ProgramState -> ProgramState
                                 , value        :: AssVar -> ProgramState -> ProgramState
                                 }

data ProgramState = ProgramState { progMem :: [ Instruction ]
                                 , functs  :: FunctionMap
                                 , resReg  :: Register
                                 , stack   :: StackFrame
                                 }

-- |This constant defines the register that is used for the program counter.
pc :: Register
pc = 0

-- |This constant defines the register that is used for the stack pointer.
sp :: Register
sp = pc + 1

-- |This constant defines an empty StackFrame.
newStackFrame :: StackFrame
newStackFrame = StackFrame { vars         = Map.empty
                           , nextFramePos = 0
                           , push         = pushF
                           , assign       = assignF
                           , value        = valueF
                           }
  where locF :: AssVar -> ProgramState -> ProgramState
        locF (AssVar i mex) ps = ps'
          { progMem = progMem ps' ++ [ LDC (r + 1) (Left var_offset)
                                     , ADD (r + 2) (r + 1) r
                                     , ADD (r + 3) (r + 2) sp
                                     ]
          , resReg  = r + 3
          }
          where ps'        = generateExpression (fromJust mex) ps
                var_offset = snd $ Map.findWithDefault (INT, 0) i $ vars $ stack ps'
                r          = resReg ps'
        pushF :: DecVar -> ProgramState -> ProgramState
        pushF   (DecVar t i mex) ps = ps
          { stack = sf { vars = Map.insert i (t, nextFramePos sf) $ vars sf
                       , nextFramePos = nextFramePos sf + size
                       }
          }
          where sf           = stack ps
                (Const size) = fromJust mex
        assignF :: Assign -> ProgramState -> ProgramState
        assignF (Assign av ex) ps = ps''
          { progMem = progMem ps'' ++ [ STM (resReg ps') (resReg ps'') ]
          }
          where ps'  = locF av ps
                ps'' = generateExpression ex ps'
        valueF :: AssVar -> ProgramState -> ProgramState
        valueF  av ps = ps
          { progMem = progMem ps' ++ [ LDM (r + 1) (resReg ps') ]
          , resReg  = r + 1
          }
          where ps' = locF av ps
                r   = resReg ps'

-- |This constant defines an empty ProgramState.
emptyProgramState :: ProgramState
emptyProgramState = ProgramState { progMem = []
                                 , functs  = Map.empty
                                 , resReg  = sp + 1
                                 , stack   = newStackFrame
                                 }

generate :: Program -> [ Instruction ]
generate = undefined

generateProgram :: Program -> ProgramState -> ProgramState
generateProgram = undefined

generateFunction :: Function -> ProgramState -> ProgramState
generateFunction = undefined

generateStatements :: [ Statement ] -> ProgramState -> ProgramState
generateStatements = flip $ foldr generateStatement

-- |This function generates the program state for a statement.
generateStatement :: Statement -> ProgramState -> ProgramState
generateStatement (Declaration d) ps = generateDecVar d ps
generateStatement (Assignment  a) ps = generateAssign a ps
generateStatement (AssignDeclr ad) ps = generateAssignDecl ad ps
generateStatement (Cond  ex s1 s2) ps = ps' { progMem = insts
                                                      ++ [ BEZ (Right else_l) (resReg ps')
                                                         ]
                                                      ++ s1_insts
                                                      ++ [ LDC (resReg ps''' + 1) (Right end_l)
                                                         , JMP (resReg ps''' + 1)
                                                         ]
                                                      ++ [ LABEL else_l
                                                         ]
                                                      ++ s2_insts
                                                      ++ [ LABEL end_l
                                                         ]
                                            , resReg  = (resReg ps''' + 1)
                                            }
  where ps'      = generateExpression ex ps
        insts    = progMem ps'
        ps''     = generateStatements s1 (ps' { progMem = [] })
        s1_insts = progMem ps''
        ps'''    = generateStatements s2 (ps' { progMem = [], resReg = resReg ps'' })
        s2_insts = progMem ps'''
        else_l   = "l" ++ show (resReg ps')
        end_l    = "l" ++ show (resReg ps' + 1)
generateStatement (While ex st) ps = ps' { progMem = insts
                                                   ++ [ LABEL start_l
                                                      ]
                                                   ++ ex_insts
                                                   ++ [ BEZ (Right end_l) (resReg ps')
                                                      ]
                                                   ++ st_insts
                                                   ++ [ LDC (resReg ps'' + 1) (Right start_l)
                                                      , JMP (resReg ps'' + 1)
                                                      , LABEL end_l
                                                      ]
                                         }
  where insts    = progMem ps
        ps'      = generateExpression ex (ps { progMem = [] })
        ex_insts = progMem ps'
        ps''     = generateStatements st (ps' { progMem = [] })
        st_insts = progMem ps''
        start_l  = "l" ++ show (resReg ps')
        end_l    = "l" ++ show (resReg ps' + 1)
--generateStatement (For ad ex a st) s = undefined --TODO
generateStatement (FunctionCall fc) ps = generateFuncCall fc ps
--generateStatement (Return      ex) s = undefined --TODO

generateDecVar :: DecVar -> ProgramState -> ProgramState
generateDecVar dv ps = push (stack ps) dv ps

generateAssign :: Assign -> ProgramState -> ProgramState
generateAssign as ps = assign (stack ps) as ps

generateAssignDecl :: AssignDecl -> ProgramState -> ProgramState
generateAssignDecl (AssignDecl dv ex) ps = assign sf (Assign (decToAss dv) ex) . push sf dv $ ps
  where decToAss (DecVar _ i mex) = AssVar i mex
        sf                        = stack ps

generateExpression :: Expression -> ProgramState -> ProgramState
generateExpression ex ps = case ex of
  (TRUE       ) -> genConst 1 ps
  (FALSE      ) -> genConst 0 ps
  (Const i    ) -> genConst i ps
  (Func  fc   ) -> generateFuncCall fc ps
  (Var   v    ) -> value (stack ps) v ps
  (Add   e1 e2) -> genBin ADD e1 e2 ps
  (Sub   e1 e2) -> genBin SUB e1 e2 ps
  (Mul   e1 e2) -> genBin MUL e1 e2 ps
  (Div   e1 e2) -> genBin DIV e1 e2 ps
  (Eq    e1 e2) -> genBin CEQ e1 e2 ps
  (Lt    e1 e2) -> genUni NOT (Or (Eq e1 e2) (Gt e1 e2)) ps
  (Gt    e1 e2) -> genBin CGT e1 e2 ps
  (Lte   e1 e2) -> genUni NOT (Gt e1 e2) ps
  (Gte   e1 e2) -> genUni NOT (Lt e1 e2) ps
  (Neg   e1   ) -> genUni NOT e1    ps
  (And   e1 e2) -> genBin AND e1 e2 ps
  (Or    e1 e2) -> genBin OR  e1 e2 ps

generateFuncCall :: FuncCall -> ProgramState -> ProgramState
generateFuncCall (FuncCall i args) ps = ps' { progMem = insts
                                                      ++ save_registers
                                                      ++ [ LDC (r + 1) (Left old_sf_size)
                                                         , ADD sp sp (r + 1)
                                                         ]
                                                      ++ push_return_addr
                                                      ++ push_return_val
                                                      ++ push_params
                                                      ++ [ LDC (r + 2) (Right i)
                                                         , JMP (r + 2)
                                                         ]
                                                      ++ load_registers
                                                      ++ [ LDM (r + 3) return_val
                                                         ]
                                                      ++ [ LDC (r + 4) (Left old_sf_size)
                                                         , SUB sp sp (r + 4)
                                                         ]
                                            , resReg  = r + 4
                                            }
  where insts = progMem ps
        ps' = ps { stack = newStackFrame }
        old_stack = stack ps
        old_sf_size = nextFramePos old_stack
        return_val = undefined
        r = undefined
        save_registers = undefined--foldr (\i ps -> ) ps [(sp + 1)..15]
        push_return_addr = undefined
        push_return_val = undefined
        push_params = undefined
        load_registers = undefined--foldr (\i ps -> ) ps [(sp + 1)..15]

-- |This function updates @s@ with the instructions for loading the constant
--  @i@ into a register.
genConst :: Int -> ProgramState -> ProgramState
genConst i ps = ps { progMem = insts ++ [ LDC (r + 1) (Left i) ]
                   , resReg  = r + 1
                   }
  where r     = resReg ps
        insts = progMem ps

-- |This function updates @ps@ with the instructions for executing @ex@ and
--  performing @cons@ on the result.
genUni :: (Register -> Register -> Instruction) ->
          Expression -> ProgramState -> ProgramState
genUni cons ex ps = ps' { progMem = insts ++ [ cons (r + 1) r ]
                        , resReg  = r + 1
                        }
  where ps'   = generateExpression ex ps
        r     = resReg ps'
        insts = progMem ps'

-- |This function updates @ps@ with the instructions for executing @e1@ and @e2@
--  and performing @cons@ on the result.
genBin :: (Register -> Register -> Register -> Instruction) ->
          Expression -> Expression -> ProgramState -> ProgramState
genBin cons e1 e2 ps = ps'' { progMem = insts ++ [ cons (r' + 1) r r' ]
                            , resReg  = r' + 1
                            }
  where ps'   = generateExpression e1 ps
        ps''  = generateExpression e2 ps'
        r     = resReg ps'
        r'    = resReg ps''
        insts = progMem ps''

{-
-- |This function puts all function identifiers with their associated argument
--  numbers into the FunctionMap.
liftFunctions :: Program -> ProgramState -> ProgramState
liftFunctions p ps = foldr insertFunc ps p
  where insertFunc (Function t i args) s = s { functs = Map.insert i (t, ats args, ids args) functs s }
        ats     = map (\(Arg t _) -> t)
        ids     = map (\(Arg _ i) -> i)
        err i _ = error "The function " ++ show i ++ " has already been declared."

-- |This function generates the code for a program.
generate :: Program -> [ Instruction ]
generate prog = sp_inst ++ jmp_insts ++ insts
  where s'        = generateProgram prog emptyProgramState
        insts     = progMem s'
        jmp_insts = [ LDC (resReg s') (Right "main")
                    , JMP (resReg s') ]
        sp_inst   = [ LDC sp (Left $ length (progMem s')
                                   + length (globMem s')
                                   + length jmp_insts   ) ]
        --TODO: Add offset to all memory accesses.

-- |This function generates the program state for a program.
generateProgram :: Program -> ProgramState -> ProgramState
generateProgram prog s = foldr generateFunction s prog

-- |This function generates the program state for a function.
generateFunction :: Function -> ProgramState -> ProgramState
generateFunction func s = undefined
-- Need to check that all return statements are of the correct type.

-- |This function generates the program state for a statement.
generateStatement :: Statement -> ProgramState -> ProgramState
generateStatement (Declaration d ) s = generateDecVar     d  s
generateStatement (Assignment  a ) s = generateAssign     a  s
generateStatement (AssignDeclr ad) s = generateAssignDecl ad s
generateStatement (Cond  ex s1 s2) s = undefined --TODO
generateStatement (While ex s1   ) s = undefined --TODO
generateStatement (For ad ex a st) s = undefined --TODO
generateStatement (FunctionCall f) s = generateFuncCall   f  s
generateStatement (Return      ex) s = undefined --TODO

-- |This function generates the program state for a declaration statement.
generateDecVar :: DecVar -> ProgramState -> ProgramState
generateDecVar (DecVar t i mex) s = s { globMem = globMem'
                                      , vars    = vars'
                                      }
  where (Const c) = extractArrOffset (functs s) (vars s) mex 1
        vars'     = Map.insert i (t, length $ globMem s) $ vars s
        globMem'  = globMem s ++ replicate c "i"

-- |This function generates the program state for a variable assignment.
generateAssign :: Assign -> ProgramState -> ProgramState
generateAssign (Assign v@(AssVar i mex) ex) s = s'' { progMem = insts ++ insts'
                                                    , resReg  = r + 2
                                                    }
  where ex'  = checkType (functs s) (vars s) ex (extractVType (vars s) i)
                        "Type of expression doesn't match type of variable."
        mex' = extractArrOffset (functs s) (vars s) mex 0
        s' = generateExpression ex' s
        ex_res = resReg s'
        s'' = generateExpression mex' s
        r = resReg s''
        insts = progMem s''
        insts' = [ LDC (r + 1) (Left $ extractVAddress (vars s'') i)
                 , ADD (r + 2) r (r + 1)
                 , STM (r + 2) ex_res
                 ]

-- |This function generates the program state for a variable declaration
--  and assignment.
generateAssignDecl :: AssignDecl -> ProgramState -> ProgramState
generateAssignDecl (AssignDecl v@(DecVar _ i mex) e) s = s''
  where s'  = generateDecVar v s
        s'' = generateAssign (Assign (AssVar i mex) e) s

-- |This function generates the program state for a function call.
generateFuncCall :: FuncCall -> ProgramState -> ProgramState
generateFuncCall (FuncCall i args) s = undefined
-}
