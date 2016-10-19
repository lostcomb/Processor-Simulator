-- |This module defines an assembler for the simple instruction set.

module Assembler
  ( assemble
  , assembleBinary
  ) where

import Assembly.Instruction

import Data.Bits
import qualified Data.Map.Strict as Map

type LabelMap = Map.Map Label Constant

-- |This function replaces labels with their physical addresses.
assemble :: [ Instruction ] -> [ Instruction ]
assemble insts = map (replaceLabel m) . filter (not . isLabel) $ insts
  where m = findLabels insts

-- |This function replaces labels with their physical addresses and puts the
--  instructions in to their binary representation.
assembleBinary :: [ Instruction ] -> [ Constant ]
assembleBinary insts = instsToBinary m . filter (not . isLabel) $ insts
  where m = findLabels insts

-- |This function returns True if the specified instruction is a LABEL.
isLabel :: Instruction -> Bool
isLabel (LABEL _) = True
isLabel _         = False

-- |This function puts all labels and their addresses in to a Map.
findLabels :: [ Instruction ] -> LabelMap
findLabels = fst . foldl updateMap (Map.empty, 0)

-- |If the specified instruction is a LABEL, this function updates @m@ with
--  its address, else it increments @pc@.
updateMap :: (LabelMap, Constant) -> Instruction -> (LabelMap, Constant)
updateMap (m, pc) (LABEL l) = (Map.insert l (pc + 1) m, pc    )
updateMap (m, pc) _         = (                      m, pc + 1)

-- |This function replaces Labels in the instrction @i@ with their physical
--  addresses.
replaceLabel :: LabelMap -> Instruction -> Instruction
replaceLabel m i = case i of
  (LDC rd (Right l)) -> LDC rd (Left $ labelToConst m l)
  (BEZ (Right l) ri) -> BEZ (Left $ labelToConst m l) ri
  _                  -> i

-- |This function returns the binary form of the specified instructons.
instsToBinary :: LabelMap -> [ Instruction ] -> [ Constant ]
instsToBinary m = map (instToBinary m)

-- |This function returns the binary form of the specified instrcution.
instToBinary :: LabelMap -> Instruction -> Constant
-- Arithmetic
instToBinary m (ADD rd ri        rj) = argsToBinary 1  rd ri rj 0
instToBinary m (SUB rd ri        rj) = argsToBinary 2  rd ri rj 0
instToBinary m (MUL rd ri        rj) = argsToBinary 3  rd ri rj 0
instToBinary m (DIV rd ri        rj) = argsToBinary 4  rd ri rj 0
-- Logic
instToBinary m (AND rd ri        rj) = argsToBinary 5  rd ri rj 0
instToBinary m (OR  rd ri        rj) = argsToBinary 6  rd ri rj 0
instToBinary m (NOT rd ri          ) = argsToBinary 8  rd ri 0  0
-- Control Flow
instToBinary m (JMP            ri  ) = argsToBinary 9  0  0  0  ri
instToBinary m (BEZ (Left  c ) ri  ) = argsToBinary 10 0  ri 0  c
instToBinary m (BEZ (Right l ) ri  ) = argsToBinary 10 0  ri 0  (labelToConst m l)
-- Comparion
instToBinary m (CEQ rd ri        rj) = argsToBinary 11 rd ri rj 0
instToBinary m (CGT rd ri        rj) = argsToBinary 12 rd ri rj 0
-- Load/Store
instToBinary m (LDC rd (Left  c)   ) = argsToBinary 13 rd 0  0  c
instToBinary m (LDC rd (Right l)   ) = argsToBinary 13 rd 0  0  (labelToConst m l)
instToBinary m (LDM rd ri          ) = argsToBinary 14 rd ri 0  0
instToBinary m (STM ri           rj) = argsToBinary 15 0  ri rj 0
-- Misc
instToBinary m (NOP                ) = argsToBinary 16 0  0  0  0

-- |This function concatenates all of the components of an instruction in binary
--  representation.
argsToBinary :: Constant -> Register -> Register -> Register -> Constant -> Constant
argsToBinary a d i j c =   shiftL (a .&. 15) 28
                       .|. shiftL (d .&. 15) 24
                       .|. shiftL (i .&. 15) 20
                       .|. shiftL (j .&. 15) 16
                       .|.        (c .&. 15)

-- |This function returns the physical address for @l@. If @l@ is not yet
--  defined, 'error' is called.
labelToConst :: LabelMap -> Label -> Constant
labelToConst m l = case Map.lookup l m of
                    Just w  -> w
                    Nothing -> error $ "Undefined Label: " ++ show l ++ "."
