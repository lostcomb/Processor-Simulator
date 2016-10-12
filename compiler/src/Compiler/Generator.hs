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

data ProgramState = ProgramState { progMem  :: [ Instruction ] --Program Memory.
                                 , globMem  :: [ Identifier  ] --Globals Memory.
                                 , functs   :: FunctionMap     --Maintains a map of functions to their
                                                               --type and the list of argument types.
                                 , vars     :: VariableMap     --Maintains a map of the variables
                                                               --currently in scope to their type and
                                                               --where they are stored.
                                 , availReg :: Register        --Stores the next available register.
                                 }

-- |This constant defines the register that is used for the program counter.
pc :: Register
pc = 0

-- |This constant defines the register that is used for the stack pointer.
sp :: Register
sp = pc + 1

-- |This constant defines an empty ProgramState.
emptyProgramState :: ProgramState
emptyProgramState = ProgramState { progMem  = []
                                 , globMem  = []
                                 , functs   = Map.empty
                                 , vars     = Map.empty
                                 , availReg = sp + 1
                                 }

-- |This function generates the code for a program.
generate :: Program -> ([ Instruction ])
generate prog = undefined --In this function, add code to set the stack pointer, and jump to the main function.

generateProgram :: ProgramState -> Program -> ProgramState
generateProgram = undefined

generateFunction :: Function -> ProgramState -> ProgramState
generateFunction = undefined

generateStatement :: Statement -> ProgramState -> ProgramState
generateStatement = undefined

generateDecVar :: ProgramState -> DecVar -> ProgramState
generateDecVar = undefined

generateAssVar :: ProgramState -> AssVar -> ProgramState
generateAssVar = undefined

generateAssign :: ProgramState -> Assign -> ProgramState
generateAssign = undefined

generateAssignDecl :: ProgramState -> AssignDecl -> ProgramState
generateAssignDecl = undefined

generateFuncCall :: ProgramState -> FuncCall -> ProgramState
generateFuncCall = undefined

generateExpression :: ProgramState -> Expression -> ProgramState
generateExpression s e = case e of
  (TRUE    ) -> genConst s 1
  (FALSE   ) -> genConst s 0
  (Const i ) -> genConst s i
  (Func  fc) -> generateFuncCall s fc
  (Var (AssVar i mex)) -> undefined --TODO
  (Add e1 e2) -> genBin s ADD e1 e2
  (Sub e1 e2) -> genBin s SUB e1 e2
  (Mul e1 e2) -> genBin s MUL e1 e2
  (Div e1 e2) -> genBin s DIV e1 e2
  (Eq  e1 e2) -> undefined --TODO
  (Lt  e1 e2) -> undefined --TODO
  (Gt  e1 e2) -> undefined --TODO
  (Lte e1 e2) -> undefined --TODO
  (Gte e1 e2) -> undefined --TODO
  (Neg e1   ) -> genUni s NOT e1
  (And e1 e2) -> genBin s AND e1 e2
  (Or  e1 e2) -> genBin s OR  e1 e2

-- |This function updates @s@ with the instructions for loading the constant
--  @i@ into a register.
genConst :: ProgramState -> Int -> ProgramState
genConst s i = s { progMem  = insts ++ [ inst ]
                 , availReg = r'
                 }
  where r'    = availReg s + 1
        insts = progMem s
        inst  = LDC r' (Left i)

-- |This function updates @s@ with the instructions for executing @e@ and
--  performing @cons@ on the result.
genUni :: ProgramState -> UniOpCons -> Expression -> ProgramState
genUni s cons e = s' { progMem  = insts ++ [ inst ]
                     , availReg = r'
                     }
  where s' = generateExpression s e
        r' = availReg s' + 1
        insts = progMem s'
        inst  = cons r' (availReg s')

-- |This function updates @s@ with the instructions for executing @e1@ and @e2@
--  and performing @cons@ on the result.
genBin :: ProgramState -> BinOpCons -> Expression -> Expression -> ProgramState
genBin s cons e1 e2 = s' { progMem  = insts ++ [ inst ]
                         , availReg = r'
                         }
  where s'    = generateExpression s  e1
        s''   = generateExpression s' e2
        r'    = availReg s'' + 1
        insts = progMem s''
        inst  = cons r' (availReg s') (availReg s'')

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
  (Add e1 e2) -> if expressionType f v e1 == INT && expressionType f v e2 == INT
                   then INT
                   else error $ "Operand(s) to Add are not of type INT."
  (Sub e1 e2) -> if expressionType f v e1 == INT && expressionType f v e2 == INT
                   then INT
                   else error $ "Operand(s) to Sub are not of type INT."
  (Mul e1 e2) -> if expressionType f v e1 == INT && expressionType f v e2 == INT
                   then INT
                   else error $ "Operand(s) to Mul are not of type INT."
  (Div e1 e2) -> if expressionType f v e1 == INT && expressionType f v e2 == INT
                   then INT
                   else error $ "Operand(s) to Div are not of type INT."
  (Eq  e1 e2) -> if expressionType f v e1 == expressionType f v e2
                   then BOOL
                   else error $ "Type of operands to Eq do not match."
  (Lt  e1 e2) -> if expressionType f v e1 == INT && expressionType f v e2 == INT
                   then BOOL
                   else error $ "Operand(s) to Lt are not of type INT."
  (Gt  e1 e2) -> if expressionType f v e1 == INT && expressionType f v e2 == INT
                   then BOOL
                   else error $ "Operand(s) to Gt are not of type INT."
  (Lte e1 e2) -> if expressionType f v e1 == INT && expressionType f v e2 == INT
                   then BOOL
                   else error $ "Operand(s) to Lte are not of type INT."
  (Gte e1 e2) -> if expressionType f v e1 == INT && expressionType f v e2 == INT
                   then BOOL
                   else error $ "Operand(s) to Gte are not of type INT."
  (Neg ex   ) -> if expressionType f v ex == BOOL
                   then BOOL
                   else error $ "Operand to Neg is not of type BOOL."
  (And e1 e2) -> if expressionType f v e1 == BOOL && expressionType f v e2 == BOOL
                   then BOOL
                   else error $ "Operand(s) to And are not of type BOOL."
  (Or  e1 e2) -> if expressionType f v e1 == BOOL && expressionType f v e2 == BOOL
                   then BOOL
                   else error $ "Operand(s) to Or are not of type BOOL."

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
