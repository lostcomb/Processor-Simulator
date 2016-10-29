{-# LANGUAGE GADTs #-}

module Components.Instructions
  ( Instruction(..)
  ) where

import Data.Int
import Components.Registers

type Constant = Int16

data Instruction a where
  Nop  :: a                                                 -> Instruction a

  Add  :: a -> Register -> Register -> Register             -> Instruction a
  Sub  :: a -> Register -> Register -> Register             -> Instruction a
  Mul  :: a -> Register -> Register -> Register             -> Instruction a
  Div  :: a -> Register -> Register -> Register             -> Instruction a

  And  :: a -> Register -> Register -> Register             -> Instruction a
  Or   :: a -> Register -> Register -> Register             -> Instruction a
  Not  :: a -> Register -> Register                         -> Instruction a

  Jmp  :: a             -> Register                         -> Instruction a
  Bez  :: a             -> Register             -> Constant -> Instruction a

  Ceq  :: a -> Register -> Register -> Register             -> Instruction a
  Cgt  :: a -> Register -> Register -> Register             -> Instruction a

  Ldc  :: a -> Register                         -> Constant -> Instruction a
  Ldm  :: a -> Register -> Register                         -> Instruction a
  Stm  :: a             -> Register -> Register             -> Instruction a

  Halt ::                                                      Instruction a
  deriving (Show, Eq, Read)

instance Functor Instruction where
  fmap f (Add  x rd ri rj) = Add  (f x) rd ri rj
  fmap f (Sub  x rd ri rj) = Sub  (f x) rd ri rj
  fmap f (Mul  x rd ri rj) = Mul  (f x) rd ri rj
  fmap f (Div  x rd ri rj) = Div  (f x) rd ri rj
  fmap f (And  x rd ri rj) = And  (f x) rd ri rj
  fmap f (Or   x rd ri rj) = Or   (f x) rd ri rj
  fmap f (Not  x rd ri   ) = Not  (f x) rd ri
  fmap f (Jmp  x    ri   ) = Jmp  (f x)    ri
  fmap f (Bez  x c  ri   ) = Bez  (f x) c  ri
  fmap f (Ceq  x rd ri rj) = Ceq  (f x) rd ri rj
  fmap f (Cgt  x rd ri rj) = Cgt  (f x) rd ri rj
  fmap f (Ldc  x rd c    ) = Ldc  (f x) rd c
  fmap f (Ldm  x rd ri   ) = Ldm  (f x) rd ri
  fmap f (Stm  x    ri rj) = Stm  (f x)    ri rj
  fmap f (Nop  x         ) = Nop  (f x)
  fmap f (Halt           ) = Halt
