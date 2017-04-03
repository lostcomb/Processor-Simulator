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

-- |These types define stack pointer offsets and stack pointers.
type SOffset  = Constant
type SPointer = Constant

-- |This data type defines the state needed to generate a program.
data Generator = Generator
  { labelNo      :: Int
  , registerNo   :: Int
  , instructions :: [ Instruction ]
  -- Left stores SOffset, Right stores SPointer.
  , stack        :: [ Either Identifier Identifier ]
  , functions    :: [ (Identifier, [ Arg ]) ]
  }
  deriving (Show, Eq, Read)

-- |This function returns an empty generator state.
newGenerator :: Generator
newGenerator = Generator
  { labelNo      = 0
  , registerNo   = gprb
  , instructions = []
  , stack        = []
  , functions    = []
  }

-- |This function returns a new label.
getLabel :: State Generator Label
getLabel = do s <- get
              put $ s { labelNo = labelNo s + 1 }
              return $ "l" ++ show (labelNo s + 1)

-- |This function returns a new register.
getRegister :: State Generator Register
getRegister = do s <- get
                 put $ s { registerNo = registerNo s + 1 }
                 return $ registerNo s

-- |This function sets the value to be returned during the next getRegister
--  call.
setRegister :: Int -> State Generator ()
setRegister r = do s <- get
                   put $ s { registerNo = r }

-- |This function adds @i@ to the list of instructions.
addInst :: Instruction -> State Generator ()
addInst i = do s <- get
               put $ s { instructions = instructions s ++ [i] }

-- |This function returns the list of instructions.
getInsts :: State Generator [ Instruction ]
getInsts = do s <- get
              return $ instructions s

-- |This function returns the current stack size.
getStackSize :: State Generator Int
getStackSize = do s <- get
                  return $ 4 * (length $ stack s)

-- |This function resets the stack, i.e. it makes it the empty stack.
resetStack :: State Generator ()
resetStack = do s <- get
                put $ s { stack = [] }

-- |This function adds an argument to the stack.
declareArgument :: Arg -> State Generator ()
declareArgument (Arg _ i isArray) = do s <- get
                                       if isArray
                                         then put $ s { stack = stack s ++ [Right i] }
                                         else put $ s { stack = stack s ++ [Left  i] }

-- |This function adds a variable to the stack.
declareVariable :: Variable -> State Generator ()
declareVariable (Var i)
  = do s <- get
       put $ s { stack = stack s ++ [Left i] }
declareVariable (Arr i (Const size))
  = do s <- get
       put $ s { stack = stack s ++ replicate size (Left i) }

-- |This function returns the offset to the value or pointer to the specified
--  variable @v@.
getOffset :: Variable -> State Generator (Either SOffset SPointer)
getOffset v = do s <- get
                 let i = case v of
                           (Var i  ) -> i
                           (Arr i _) -> i
                     index = fromJust . findIndex (eqVar i) $ (stack s)
                 return $ identToInt (index * 4) (stack s !! index)

-- |This function returns true if the specified identifier is equal to the
--  identifier in the either.
eqVar :: Identifier -> Either Identifier Identifier -> Bool
eqVar i (Left  i') = i == i'
eqVar i (Right i') = i == i'

-- |This function converts an either identifier to an either int.
identToInt :: Int -> Either Identifier Identifier -> Either Int Int
identToInt i (Left  _) = Left  i
identToInt i (Right _) = Right i

-- |This function adds the specified function to the list of functions.
addFunction :: Function -> State Generator ()
addFunction (Function _ i args _)
  = do s <- get
       put $ s { functions = functions s ++ [ (i, args) ] }

-- |This function returns the list of arguments to the function with identifier
--  @i@.
getFunctionArgs :: Identifier -> State Generator [ Arg ]
getFunctionArgs i = do s <- get
                       case lookup i (functions s) of
                         (Just args) -> return args
                         (Nothing  ) -> error "Function not defined."
