module Compiler.Generator
  (
  ) where

import Compiler.SyntaxTree
import Assembly.Instruction

generateProgram :: [ Function ] -> [ Instruction ]
generateProgram = undefined

generateFunction :: Function -> [ Instruction ]
generateFunction = undefined

generateStatement :: Statement -> [ Instruction ]
generateStatement = undefined

generateDecVar :: DecVar -> [ Instruction ]
generateDecVar = undefined

generateAssVar :: AssVar -> [ Instruction ]
generateAssVar = undefined

generateAssign :: Assign -> [ Instruction ]
generateAssign = undefined

generateAssignDecl :: AssignDecl -> [ Instruction ]
generateAssignDecl = undefined

generateFuncCall :: FuncCall -> [ Instruction ]
generateFuncCall = undefined

generateExpression :: Expression -> [ Instruction ]
generateExpression = undefined

generateAexp :: Aexp -> [ Instruction ]
generateAexp = undefined

generateBexp :: Bexp -> [ Instruction ]
generateBexp = undefined
