module Assembler
  ( assemble
  ) where

import Assembly.Instruction

import Data.Bits
import Data.Word
import qualified Data.Map.Strict as Map

type LabelMap = Map.Map Label Word32

assemble :: [ Instruction ] -> [ Word32 ]
assemble insts = instsToWord m . filter removeLabels $ insts
  where m                      = findLabels insts
        removeLabels (LABEL _) = False
        removeLabels _         = True

findLabels :: [ Instruction ] -> LabelMap
findLabels = fst . foldr updateMap (Map.empty, 0)

updateMap :: Instruction -> (LabelMap, Word32) -> (LabelMap, Word32)
updateMap (LABEL l) (m, pos) = (Map.insert l pos m, pos    )
updateMap _         (m, pos) = (                 m, pos + 1)

instsToWord :: LabelMap -> [ Instruction ] -> [ Word32 ]
instsToWord m = map (instToWord m)

instToWord :: LabelMap -> Instruction -> Word32
-- Arithmetic
instToWord m (ADD rd         ri rj  ) = argsToWord 1  rd ri rj 0
instToWord m (SUB rd         ri rj  ) = argsToWord 2  rd ri rj 0
instToWord m (MUL rd         ri rj  ) = argsToWord 3  rd ri rj 0
instToWord m (DIV rd         ri rj  ) = argsToWord 4  rd ri rj 0
-- Logic
instToWord m (AND rd         ri rj  ) = argsToWord 5  rd ri rj 0
instToWord m (OR  rd         ri rj  ) = argsToWord 6  rd ri rj 0
instToWord m (XOR rd         ri rj  ) = argsToWord 7  rd ri rj 0
instToWord m (NOT rd         ri rj  ) = argsToWord 8  rd ri rj 0
-- Control Flow
instToWord m (JMP (Left  ri)        ) = argsToWord 9  0  0  0  ri
instToWord m (JMP (Right l )        ) = argsToWord 9  0  0  0  (labelToWord m l)
instToWord m (BEQ (Left  c ) ri rj  ) = argsToWord 10 0  ri rj c
instToWord m (BEQ (Right l ) ri rj  ) = argsToWord 10 0  ri rj (labelToWord m l)
instToWord m (BGT (Left  c ) ri rj  ) = argsToWord 11 0  ri rj c
instToWord m (BGT (Right l ) ri rj  ) = argsToWord 11 0  ri rj (labelToWord m l)
instToWord m (BEZ (Left  c ) ri     ) = argsToWord 12 0  ri 0  c
instToWord m (BEZ (Right l ) ri     ) = argsToWord 12 0  ri 0  (labelToWord m l)
-- Load/Store
instToWord m (LDC rd               c) = argsToWord 13 rd 0  0  c
instToWord m (LDM rd         ri     ) = argsToWord 14 rd ri 0  0
instToWord m (STM ri            rj  ) = argsToWord 15 0  ri rj 0
-- Misc
instToWord m (NOP                   ) = argsToWord 16 0  0  0  0

argsToWord :: Word32 -> Register -> Register -> Register -> Constant -> Word32
argsToWord a d i j c =   shiftL a 28
                     .|. shiftL d 24
                     .|. shiftL i 20
                     .|. shiftL j 16
                     .|. c

labelToWord :: LabelMap -> Label -> Word32
labelToWord m l = case Map.lookup l m of
                    Just w  -> w
                    Nothing -> error $ "Undefined Label: " ++ show l ++ "."
