-- |This module generates the code from the syntax tree. It assumes there is
--  an infinite number of registers available.

module Compiler.Generator
  ( generate
  ) where

import Compiler.SyntaxTree
import Assembly.Instruction

import Data.Maybe
import qualified Data.Map.Strict as Map

type ArgNo        = Int
type Address      = Int
type SPOffset     = Constant
type FunctionMap  = Map.Map Identifier (Type, [ Type ])
type VariableMap  = Map.Map Identifier (Type, Address)
type UniOpCons    = Register -> Register -> Instruction
type BinOpCons    = Register -> Register -> Register -> Instruction

data ProgramState = ProgramState { progMem :: [ Instruction ]
                                 , globMem :: [ Identifier  ]
                                 , functs  :: FunctionMap
                                 , vars    :: VariableMap
                                 , resReg  :: Register
                                 }

-- |This constant defines the register that is used for the program counter.
pc :: Register
pc = 0

-- |This constant defines the register that is used for the stack pointer.
sp :: Register
sp = pc + 1

-- |This constant defines an empty ProgramState.
emptyProgramState :: ProgramState
emptyProgramState = ProgramState { progMem = []
                                 , globMem = []
                                 , functs  = Map.empty
                                 , vars    = Map.empty
                                 , resReg  = sp + 1
                                 }

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

generateDecVar :: DecVar -> ProgramState -> ProgramState
generateDecVar = undefined

generateAssVar :: AssVar -> ProgramState -> ProgramState
generateAssVar = undefined

generateAssign :: Assign -> ProgramState -> ProgramState
generateAssign = undefined

generateAssignDecl :: AssignDecl -> ProgramState -> ProgramState
generateAssignDecl = undefined

generateFuncCall :: FuncCall -> ProgramState -> ProgramState
generateFuncCall = undefined

-- resReg in output state is the register that the result is stored in.
generateExpression :: Expression -> ProgramState -> ProgramState
generateExpression e s = case e of
  (TRUE       ) -> genConst 1 s
  (FALSE      ) -> genConst 0 s
  (Const i    ) -> genConst i s
  (Func  fc   ) -> generateFuncCall fc s
  (Var   v    ) -> genVar   v s
  (Add   e1 e2) -> genBin ADD e1 e2 s
  (Sub   e1 e2) -> genBin SUB e1 e2 s
  (Mul   e1 e2) -> genBin MUL e1 e2 s
  (Div   e1 e2) -> genBin DIV e1 e2 s
  (Eq    e1 e2) -> genBin CEQ e1 e2 s
  (Lt    e1 e2) -> genUni NOT (Or (Eq e1 e2) (Gt e1 e2)) s
  (Gt    e1 e2) -> genBin CGT e1 e2 s
  (Lte   e1 e2) -> genUni NOT (Gt e1 e2) s
  (Gte   e1 e2) -> genUni NOT (Lt e1 e2) s
  (Neg   e1   ) -> genUni NOT e1    s
  (And   e1 e2) -> genBin AND e1 e2 s
  (Or    e1 e2) -> genBin OR  e1 e2 s

-- |This function updates @s@ with the instructions for loading the constant
--  @i@ into a register.
genConst :: Int -> ProgramState -> ProgramState
genConst i s = s { progMem = insts ++ [ inst ]
                 , resReg  = r'
                 }
  where r'    = resReg s + 1
        insts = progMem s
        inst  = LDC r' (Left i)

-- |This function updates @s@ with the instructions for reading the value of
--  the variable @i@.
genVar :: AssVar -> ProgramState -> ProgramState
genVar (AssVar i mex) s = s' { progMem = insts ++ insts'
                             , resReg  = r + 3
                             }
  where ex     = fromMaybe (Const 0) mex
        s'     = generateExpression ex s
        insts  = progMem s'
        r      = resReg s'
        insts' = [ LDC (r + 1) (Left $ extractVAddress (vars s') i)
                 , ADD (r + 2) r (r + 1)
                 , LDM (r + 3) (r + 2)
                 ]

-- |This function updates @s@ with the instructions for executing @e@ and
--  performing @cons@ on the result.
genUni :: UniOpCons -> Expression -> ProgramState -> ProgramState
genUni cons e s = s' { progMem = insts ++ [ inst ]
                     , resReg  = r'
                     }
  where s' = generateExpression e s
        r' = resReg s' + 1
        insts = progMem s'
        inst  = cons r' (resReg s')

-- |This function updates @s@ with the instructions for executing @e1@ and @e2@
--  and performing @cons@ on the result.
genBin :: BinOpCons -> Expression -> Expression -> ProgramState -> ProgramState
genBin cons e1 e2 s = s' { progMem = insts ++ [ inst ]
                         , resReg  = r'
                         }
  where s'    = generateExpression e1 s
        s''   = generateExpression e2 s'
        r'    = resReg s'' + 1
        insts = progMem s''
        inst  = cons r' (resReg s') (resReg s'')

-- |This function puts all function identifiers with their associated argument
--  numbers into the FunctionMap.
liftFunctions :: FunctionMap -> Program -> FunctionMap
liftFunctions = foldr (\(Function t ident args _) -> Map.insert ident $ (t, ts args))
  where ts = map (\(Arg t _) -> t)

-- |This function determines whether @e@ is an arithmetic
--  expression, or a boolean expression. It also throws an error if the
--  expression is not valid (i.e. a boolean variable is used in an arithmetic
--  expression).
expressionType :: FunctionMap -> VariableMap -> Expression -> Type
expressionType f v e = case e of
  (TRUE   ) -> BOOL
  (FALSE  ) -> BOOL
  (Const i) -> INT
  (Func (FuncCall i es)) -> if extractFArgTypes f i == map (expressionType f v) es
                              then extractFType f i
                              else error $ "Types of arguments to function "
                                           ++ show i
                                           ++ " don't match. Expected: "
                                           ++ show (extractFArgTypes f i)
                                           ++ ". Found: "
                                           ++ show (map (expressionType f v) es)
                                           ++ "."
  (Var  (AssVar   i ex)) -> if isNothing ex || expressionType f v (fromJust ex) == INT
                              then extractVType v i
                              else error $ "Array index expression was not of type INT."
  (Add e1 e2) -> checkBinTypes f v e1 e2 "Add" INT INT
  (Sub e1 e2) -> checkBinTypes f v e1 e2 "Sub" INT INT
  (Mul e1 e2) -> checkBinTypes f v e1 e2 "Mul" INT INT
  (Div e1 e2) -> checkBinTypes f v e1 e2 "Div" INT INT
  (Eq  e1 e2) -> if expressionType f v e1 == expressionType f v e2
                   then BOOL
                   else error $ "Type of operands to Eq do not match."
  (Lt  e1 e2) -> checkBinTypes f v e1 e2 "Lt"  INT BOOL
  (Gt  e1 e2) -> checkBinTypes f v e1 e2 "Gt"  INT BOOL
  (Lte e1 e2) -> checkBinTypes f v e1 e2 "Lte" INT BOOL
  (Gte e1 e2) -> checkBinTypes f v e1 e2 "Gte" INT BOOL
  (Neg ex   ) -> if expressionType f v ex == BOOL
                   then BOOL
                   else error $ "Operand to Neg is not of type BOOL."
  (And e1 e2) -> checkBinTypes f v e1 e2 "And" BOOL BOOL
  (Or  e1 e2) -> checkBinTypes f v e1 e2 "Or"  BOOL BOOL

-- |This function checks that the @e1@ and @e2@ have the same type, @check_t@.
--  If they do, then @return_t@ is returned, otherwise 'error' is called,
--  using @operator@ to describe where the problem is.
checkBinTypes :: FunctionMap -> VariableMap ->
                 Expression  -> Expression  -> String -> Type -> Type -> Type
checkBinTypes f v e1 e2 operator check_t return_t =
  if expressionType f v e1 == check_t && expressionType f v e2 == check_t
    then return_t
    else error $ "Operand(s) to " ++ operator ++ " are not of type " ++ show check_t ++ "."

-- |This function returns the type associated with @i@. If @i@ is not in the
--  FunctionMap @f@, 'error' is called.
extractFType :: FunctionMap -> Identifier -> Type
extractFType f i = case Map.lookup i f of
  (Just (t, _)) -> t
  (Nothing    ) -> error $ "Function " ++ show i ++ " used but not declared."

-- |This function returns the list of types associated with the arguments of
--  @i@. If @i@ is not in the FunctionMap @f@, 'error' is called.
extractFArgTypes :: FunctionMap -> Identifier -> [ Type ]
extractFArgTypes f i = case Map.lookup i f of
  (Just (_, ts)) -> ts
  (Nothing     ) -> error $ "Function " ++ show i ++ " used but not declared."

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
