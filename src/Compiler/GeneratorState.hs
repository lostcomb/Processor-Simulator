module Compiler.GeneratorState
  ( module Compiler.GeneratorState
  , module Compiler.SyntaxTree
  , module Compiler.Instruction
  ) where

import Data.List (findIndex)
import Data.Maybe (fromJust)
import Control.Monad.State

import Compiler.SyntaxTree
import Compiler.Instruction

type SOffset  = Constant
type SPointer = Constant

data Generator = Generator
  { labelNo      :: Int
  , registerNo   :: Int
  , instructions :: [ Instruction ]
  , stack        :: [ Either Identifier Identifier ] -- Left stores SOffset, Right stores SPointer
  , functions    :: [ (Identifier, [ Arg ]) ]
  }
  deriving (Show, Eq, Read)

newGenerator :: Generator
newGenerator = Generator
  { labelNo      = 0
  , registerNo   = gprb
  , instructions = []
  , stack        = []
  , functions    = []
  }

pc, sp, gprb :: Register
pc   = 0
sp   = 1
gprb = 2


getLabel :: State Generator Label
getLabel = do s <- get
              put $ s { labelNo = labelNo s + 1 }
              return $ "l" ++ show (labelNo s + 1)

getRegister :: State Generator Register
getRegister = do s <- get
                 put $ s { registerNo = registerNo s + 1 }
                 return $ registerNo s

setRegister :: Int -> State Generator ()
setRegister r = do s <- get
                   put $ s { registerNo = r }

addInst :: Instruction -> State Generator ()
addInst i = do s <- get
               put $ s { instructions = instructions s ++ [i] }

getInsts :: State Generator [ Instruction ]
getInsts = do s <- get
              return $ instructions s

getStackSize :: State Generator Int
getStackSize = do s <- get
                  return $ length $ stack s

declareArgument :: Arg -> State Generator ()
declareArgument (Arg _ i isArray) = do s <- get
                                       if isArray
                                         then put $ s { stack = stack s ++ [Right i] }
                                         else put $ s { stack = stack s ++ [Left  i] }

declareVariable :: Variable -> State Generator ()
declareVariable (Var i)
  = do s <- get
       put $ s { stack = stack s ++ [Left i] }
declareVariable (Arr i (Const size))
  = do s <- get
       put $ s { stack = stack s ++ replicate size (Left i) }


getOffset :: Variable -> State Generator (Either SOffset SPointer)
getOffset v = do s <- get
                 let i = case v of
                           (Var i  ) -> i
                           (Arr i _) -> i
                     index = fromJust . findIndex (eqVar i) $ (stack s)
                 return $ identToInt index (stack s !! index)

eqVar :: Identifier -> Either Identifier Identifier -> Bool
eqVar i (Left  i') = i == i'
eqVar i (Right i') = i == i'

identToInt :: Int -> Either Identifier Identifier -> Either Int Int
identToInt i (Left  _) = Left  i
identToInt i (Right _) = Right i

addFunction :: Function -> State Generator ()
addFunction (Function _ i args _)
  = do s <- get
       put $ s { functions = functions s ++ [ (i, args) ] }

getFunctionArgs :: Identifier -> State Generator [ Arg ]
getFunctionArgs i = do s <- get
                       case lookup i (functions s) of
                         (Just args) -> return args
                         (Nothing  ) -> error "Function not defined."
