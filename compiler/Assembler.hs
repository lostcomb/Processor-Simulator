{-# LANGUAGE BinaryLiterals #-}

module Assembler
  ( assemble
  ) where

import Data.Bits
import Data.Word
import System.IO
import Instruction
import qualified Data.Map.Strict as Map

type LabelMap = Map.Map Label Word32

assemble :: FilePath -> IO ()
assemble path = do contents <- readFile path ReadMode
                   insts    <- parse contents --TODO: Parse the file.
                   let m   = findLabels insts
                       bin = instsToBinary m insts
                   --TODO: Save as binary file.


findLabels :: [Instruction] -> LabelMap
findLabels = fst . foldr findLabel (Map.empty, 0)

findLabel :: Instruction -> (LabelMap, Word32) -> (LabelMap, Word32)
findLabel (LABEL l) (m, pos) = (Map.insert l pos m, pos    )
findLabel _         (m, pos) = (                 m, pos + 1)

instsToBinary :: LabelMap -> [Instruction] -> [Word32]
instsToBinary m = foldr (instToBinary m) []

instToBinary :: LabelMap -> Instruction -> [Word32] -> [Word32]
instToBinary m (ADD rd ri rj)        = (++) [ argsToInt 1  rd ri rj 0 ]
instToBinary m (SUB rd ri rj)        = (++) [ argsToInt 2  rd ri rj 0 ]
instToBinary m (MUL rd ri rj)        = (++) [ argsToInt 3  rd ri rj 0 ]
instToBinary m (DIV rd ri rj)        = (++) [ argsToInt 4  rd ri rj 0 ]

instToBinary m (AND rd ri rj)        = (++) [ argsToInt 5  rd ri rj 0 ]
instToBinary m (OR  rd ri rj)        = (++) [ argsToInt 6  rd ri rj 0 ]
instToBinary m (XOR rd ri rj)        = (++) [ argsToInt 7  rd ri rj 0 ]
instToBinary m (NOT rd ri rj)        = (++) [ argsToInt 8  rd ri rj 0 ]

instToBinary m (JMP (Left c))        = (++) [ argsToInt 9  0  0  0  c ]
instToBinary m (JMP (Right l))       = (++) [ argsToInt 9  0  0  0  0 ]--TODO
instToBinary m (BEQ (Left c) ri rj)  = (++) [ argsToInt 10 0  ri rj c ]
instToBinary m (BEQ (Right l) ri rj) = (++) [ argsToInt 10 0  ri rj 0 ]--TODO
instToBinary m (BGT (Left c) ri rj)  = (++) [ argsToInt 11 0  ri rj c ]
instToBinary m (BGT (Right l) ri rj) = (++) [ argsToInt 11 0  ri rj 0 ]--TODO
instToBinary m (BEZ (Left c) ri)     = (++) [ argsToInt 12 0  ri 0  c ]
instToBinary m (BEZ (Right l) ri)    = (++) [ argsToInt 12 0  ri 0  0 ]--TODO

instToBinary m (LDC rd c)            = (++) [ argsToInt 13 rd 0  0  c ]
instToBinary m (LDM rd ri)           = (++) [ argsToInt 14 rd ri 0  0 ]
instToBinary m (STM ri rj)           = (++) [ argsToInt 15 0  ri rj 0 ]

instToBinary m (NOP)                 = (++) [ argsToInt 16 0  0  0  0 ]

argsToInt :: Word32 -> Register -> Register -> Register -> Constant -> Word32
argsToInt a d i j c = shiftL a 28 .|. shiftL d 24 .|. shiftL i 20 .|. shiftL j 16 .|. c
