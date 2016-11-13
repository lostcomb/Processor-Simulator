module Compiler.GeneratorState
  ( SOffset
  , SPointer
  , GeneratorState
  , newGeneratorState
  , pc
  , sp
  , getNLabels
  , getNRegisters
  , resetRegisterNo
  , getInstructions
  , appendInstructions
  , getStackSize
  , declareArgument
  , declareVariable
  , getOffset
  , performRegisterAllocation
  , addFunction
  , getFunctionArgs
  ) where

import Compiler.Allocator
import Compiler.SyntaxTree
import Compiler.Instruction

type SOffset  = Constant
type SPointer = Constant

data GeneratorState = GeneratorState
  { labelNo      :: Int
  , registerNo   :: Register
  , instructions :: [ Instruction ]
  , stack        :: [ (Identifier, Either SOffset SPointer) ]
  , nextStackPos :: Int
  , functions    :: [ (Identifier, [ Arg ]) ]
  }
  deriving (Show, Eq, Read)

newGeneratorState :: GeneratorState
newGeneratorState = GeneratorState
  { labelNo      = 0
  , registerNo   = sp + 1
  , instructions = []
  , stack        = []
  , nextStackPos = 0
  , functions    = []
  }

pc, sp :: Register
pc = 0
sp = 1

getNLabels :: Int -> GeneratorState -> ([ Label ], GeneratorState)
getNLabels n s = (labels, s')
  where labels = [ "l" ++ show ((labelNo s) + i) | i <- [0..(n - 1)]]
        s'     = s { labelNo = labelNo s + n }

getNRegisters :: Int -> GeneratorState -> ([ Register ], GeneratorState)
getNRegisters n s = (registers, s')
  where registers = [(registerNo s) + i | i <- [0..fromIntegral (n - 1)]]
        s'        = s { registerNo = registerNo s + fromIntegral n }

resetRegisterNo :: GeneratorState -> GeneratorState
resetRegisterNo s = s { registerNo = sp + 1 }

getInstructions :: GeneratorState -> [ Instruction ]
getInstructions s = instructions s

appendInstructions :: [ Instruction ] -> GeneratorState -> GeneratorState
appendInstructions is s = s { instructions = instructions s ++ is }

getStackSize :: GeneratorState -> Int
getStackSize s = nextStackPos s

declareArgument :: Arg -> GeneratorState -> GeneratorState
declareArgument (Arg _ i isArray) s = s { stack        = stack'
                                        , nextStackPos = nextStackPos'
                                        }
  where cons          = if isArray then Right else Left
        stack'        = stack s ++ [ (i, cons $ fromIntegral $ nextStackPos s) ]
        nextStackPos' = nextStackPos s + 1

declareVariable :: Variable -> GeneratorState -> GeneratorState
declareVariable (Var i             ) s = s { stack        = stack'
                                           , nextStackPos = nextStackPos'
                                           }
  where stack'        = stack s ++ [ (i, Left $ fromIntegral $ nextStackPos s) ]
        nextStackPos' = nextStackPos s + 1
declareVariable (Arr i (Const size)) s = s { stack        = stack'
                                           , nextStackPos = nextStackPos'
                                           }
  where stack'        = stack s ++ [ (i, Left $ fromIntegral $ nextStackPos s) ]
        nextStackPos' = nextStackPos s + size

getOffset :: Variable -> GeneratorState -> Either SOffset SPointer
getOffset v s = offset
  where identifier = case v of
                       (Var i  ) -> i
                       (Arr i _) -> i
        offset     = case lookup identifier (stack s) of
                       (Just o ) -> o
                       (Nothing) -> undefined

performRegisterAllocation :: GeneratorState -> GeneratorState
performRegisterAllocation s = s { instructions = allocate . instructions $ s }

addFunction :: Function -> GeneratorState -> GeneratorState
addFunction (Function _ i args _) s = s { functions = functions' }
  where functions' = functions s ++ [ (i, args) ]

getFunctionArgs :: Identifier -> GeneratorState -> [ Arg ]
getFunctionArgs i s = case lookup i (functions s) of
                        (Just args) -> args
                        (Nothing  ) -> undefined
