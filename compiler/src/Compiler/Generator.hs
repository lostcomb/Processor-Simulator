-- |This module generates the code from the syntax tree. It assumes there is
--  an infinite number of registers available.

module Compiler.Generator
  ( generate
  ) where

import Compiler.Types
import Compiler.SyntaxTree
import Assembly.Instruction

import Data.Maybe
import qualified Data.Map.Strict as Map

generate :: Program -> [ Instruction ]
generate = undefined

{-data StackFrame   = StackFrame   { vars         :: VariableMap
                                 , nextFramePos :: SPOffset
                                 , push         :: DecVar -> ProgramState -> ProgramState
                                 , assign       :: Assign -> ProgramState -> ProgramState
                                 , assignPPC    :: Assign -> ProgramState -> ProgramState
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
                           , assignPPC    = assignPPCF
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
          where ps'        = generateExpression (fromMaybe (Const 0) mex) ps
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
        assignPPCF :: Assign -> ProgramState -> ProgramState
        assignPPCF (Assign av ex) ps = ps''
          { progMem = progMem ps'' ++ [ ADD (r + 1) pc r
                                      , STM (resReg ps') (r + 1)
                                      ]
          , resReg  = r + 1
          }
          where ps'  = locF av ps
                ps'' = generateExpression ex ps'
                r    = resReg ps''
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

-- |This function generates the code for the specified program @prog@.
generate :: Program -> [ Instruction ]
generate prog = [ LDC sp (Left $ length insts + 2)
                , LDC (resReg ps + 1) (Right "main")
                , JMP (resReg ps + 1)
                ]
              ++ insts
              ++ [ LABEL "_end"
                 ]
  where ps    = generateProgram prog emptyProgramState
        insts = progMem ps

-- |This function generates the program state for the specified program @prog@.
generateProgram :: Program -> ProgramState -> ProgramState
generateProgram prog ps = foldl
  (\s f -> generateFunction f (s { stack = newStackFrame }))
  (ps { functs = functs' })
  prog
  where functs'     = foldr addFunction (functs ps) prog
        mapArgTypes = map (\(Arg t _) -> t)
        mapArgIds   = map (\(Arg _ i) -> i)
        addFunction (Function t i args st) f =
          Map.insert i (t, mapArgTypes args, mapArgIds args) f

-- |This function generates the program state for the specified function.
generateFunction :: Function -> ProgramState -> ProgramState
generateFunction (Function t i args st) ps = generateStatements st ps'''
  where ps'   = ps { progMem = progMem ps ++ [ LABEL i ] }
        ps''  = push (stack ps') (DecVar t "_return" (Just (Const 1)))
              . push (stack ps') (DecVar INT "_return_addr" (Just (Const 1))) $ ps'
        ps''' = foldl (\s (Arg t i) -> push (stack s) (DecVar t i (Just (Const 1))) s) ps'' args

-- |This function generates the program state for the specified statements.
generateStatements :: [ Statement ] -> ProgramState -> ProgramState
generateStatements = flip $ foldl . flip $ generateStatement

-- |This function generates the program state for a statement.
generateStatement :: Statement -> ProgramState -> ProgramState
generateStatement (Declaration d) ps = generateDecVar d ps
generateStatement (Assignment  a) ps = generateAssign a ps
generateStatement (AssignDeclr ad) ps = generateAssignDecl ad ps
generateStatement (Cond  ex s1 s2) ps = ps'
  { progMem = insts
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
generateStatement (While ex st) ps = ps'
  { progMem = insts
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
generateStatement (For ad ex a st) ps = ps'
  { progMem = insts
            ++ [ LABEL start_l
               ]
            ++ ex_insts
            ++ [ BEZ (Right end_l) (resReg ps'')
               ]
            ++ st_insts
            ++ a_insts
            ++ [ LDC (resReg ps'' + 1) (Right start_l)
               , JMP (resReg ps'' + 1)
               , LABEL end_l
               ]
  }
  where insts = progMem ps
        ps'      = if isJust ad
                     then generateAssignDecl (fromJust ad) ps
                     else ps
        ps''     = if isJust ex
                     then generateExpression (fromJust ex) (ps' { progMem = [] })
                     else ps' { progMem = [] }
        ex_insts = progMem ps''
        a_insts  = if isJust a
                     then progMem $ generateAssign (fromJust a) (ps' { progMem = [] })
                     else []
        st_insts = progMem $ generateStatements st (ps' { progMem = [] })
        start_l  = "l" ++ show (resReg ps')
        end_l    = "l" ++ show (resReg ps' + 1)
generateStatement (FunctionCall fc) ps = generateFuncCall fc ps
generateStatement (Return ex) ps = ps''
  { progMem = progMem ps''
            ++ [ BEZ (Right "_end") (resReg ps'')
               , JMP $ resReg ps''
               ]
  }
  where ps'  = assign (stack ps) (Assign (AssVar "_return" (Just (Const 0))) ex) ps
        ps'' = value (stack ps') (AssVar "_return_addr" (Just (Const 0))) ps'

-- |This function generates the program state for the specified variable
--  declaration @dv@.
generateDecVar :: DecVar -> ProgramState -> ProgramState
generateDecVar dv ps = push (stack ps) dv ps

-- |This function generates the program state for the specified variable
--  assignment @as@.
generateAssign :: Assign -> ProgramState -> ProgramState
generateAssign as ps = assign (stack ps) as ps

-- |This function generates the program state for the specified variable
--  declaration and assignment.
generateAssignDecl :: AssignDecl -> ProgramState -> ProgramState
generateAssignDecl (AssignDecl dv ex) ps = assign sf (Assign (decToAss dv) ex) . push sf dv $ ps
  where decToAss (DecVar _ i mex) = AssVar i mex
        sf                        = stack ps

-- |This function generates the program state for the specified expression @ex@.
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

-- |This function generates the program state for the specified function call.
generateFuncCall :: FuncCall -> ProgramState -> ProgramState
generateFuncCall (FuncCall i args) ps = ps
  { progMem = insts
            ++ [ LDC (r + 1) (Left old_sf_size)
               , ADD sp sp (r + 1)
               ]
            ++ push_return_addr
            ++ push_params
            ++ [ LDC (r + 2) (Right i)
               , JMP (r + 2)
               ]
            ++ progMem ps'''''
            ++ [ LDC (r + 4) (Left old_sf_size)
               , SUB sp sp (r + 4)
               ]
  , resReg  = resReg ps'''''
  }
  where insts            = progMem ps
        old_sf_size      = nextFramePos $ stack ps
        ps'              = ps { progMem = [], stack = newStackFrame }
        (Just (fType, argTypes, argNames)) = Map.lookup i (functs ps)
        ps''             = push (stack ps')
                                (DecVar fType "_return" (Just (Const 1)))
                         . push (stack ps')
                                (DecVar INT "_ret_addr" (Just (Const 1))) $ ps'
        ps'''            = assignPPC (stack ps'')
                                     (Assign (AssVar "_return_addr" (Just (Const 0))) pc_loc) ps''
        push_return_addr = progMem ps'''
        params           = zip3 argTypes argNames args
        ps''''           = foldl (\s (t, i, e) ->
                             assign (stack s)
                                    (Assign (AssVar i (Just (Const 0))) e)
                           . push   (stack s)
                                    (DecVar t i (Just (Const 1))) $ s) ps' params
        push_params      = progMem ps''''
        pc_loc           = Const $ length push_params + 5 --This magic number is
                                                          --the number of
                                                          --instructions required
                                                          --to assign the return
                                                          --address.
        r                = resReg ps''''
        ps'''''          = value (stack ps'''')
                                 (AssVar "_return_addr" (Just (Const 0)))
                                 (ps'''' { progMem = [] })

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
-}
