{-# LANGUAGE BinaryLiterals #-}

module Assembler
  ( --assemble
  ) where

import Instruction
import qualified Data.Map.Strict as Map

type LabelMap = Map.Map Label Int

--assemble :: String -> IO ()
--assemble insts = do

findLabels :: [Instruction] -> LabelMap
findLabels = fst . foldr findLabel (Map.empty, 0)

findLabel :: Instruction -> (LabelMap, Int) -> (LabelMap, Int)
findLabel (LABEL l) (m, pos) = (Map.insert l pos m, pos    )
findLabel _         (m, pos) = (                 m, pos + 1)

instsToBinary :: LabelMap -> [Instruction] -> [Int]
instsToBinary m = foldr (instToBinary m) []

instToBinary :: LabelMap -> Instruction -> [Int] -> [Int]
instToBinary m (ADD rd ri rj)        = (++) [ 0b000000 ]
instToBinary m (SUB rd ri rj)        = (++) [ 0b000000 ]
instToBinary m (MUL rd ri rj)        = (++) [ 10 ]
instToBinary m (DIV rd ri rj)        = (++) [ 10 ]

instToBinary m (AND rd ri rj)        = (++) [ 10 ]
instToBinary m (OR  rd ri rj)        = (++) [ 10 ]
instToBinary m (XOR rd ri rj)        = (++) [ 10 ]
instToBinary m (NOT rd ri rj)        = (++) [ 10 ]

instToBinary m (JMP (Left c))        = (++) [ 10 ]
instToBinary m (JMP (Right l))       = (++) [ 10 ]
instToBinary m (BEQ (Left c) ri rj)  = (++) [ 10 ]
instToBinary m (BEQ (Right l) ri rj) = (++) [ 10 ]
instToBinary m (BGT (Left c) ri rj)  = (++) [ 10 ]
instToBinary m (BGT (Right l) ri rj) = (++) [ 10 ]
instToBinary m (BEZ (Left c) ri)     = (++) [ 10 ]
instToBinary m (BEZ (Right l) ri)    = (++) [ 10 ]

instToBinary m (LDC rd c)            = (++) [ 10 ]
instToBinary m (LDM rd ri)           = (++) [ 10 ]
instToBinary m (STM ri rj)           = (++) [ 10 ]

instToBinary m (NOP)                 = (++) [ 10 ]
