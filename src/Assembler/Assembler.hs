-- |This module defines an assembler for the simple instruction set.

module Assembler.Assembler
  ( assemble
  , assembleBinary
  ) where

import Assembler.Parser
import Assembler.Instruction

import Data.Bits
import Data.Word
import qualified Data.Map.Strict as Map

type LabelMap = Map.Map Label Constant

-- |This function replaces labels with their physical addresses.
assemble :: [ Instruction ] -> [ Instruction ]
assemble insts = map (replaceLabel m) . filter (not . isLabel) $ insts
  where m = findLabels insts

-- |This function replaces labels with their physical addresses and puts the
--  instructions in to their binary representation.
assembleBinary :: [ Instruction ] -> [ Word8 ]
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
updateMap (m, pc) (LABEL l) = (Map.insert l pc m, pc    )
updateMap (m, pc) _         = (                m, pc + 1)

-- |This function replaces Labels in the instrction @i@ with their physical
--  addresses.
replaceLabel :: LabelMap -> Instruction -> Instruction
replaceLabel m i = case i of
  (LDC rd (Right l)) -> LDC rd (Left $ labelToConst m l)
  (BEZ ri (Right l)) -> BEZ ri (Left $ labelToConst m l)
  _                  -> i

-- |This function returns the binary form of the specified instructons.
instsToBinary :: LabelMap -> [ Instruction ] -> [ Word8 ]
instsToBinary m = concat . map (instToBinary m)

-- |This function returns the binary form of the specified instrcution.
instToBinary :: LabelMap -> Instruction -> [ Word8 ]
-- Arithmetic
instToBinary m (ADD rd ri        rj) = argsToBinary 1  rd ri rj 0
instToBinary m (SUB rd ri        rj) = argsToBinary 2  rd ri rj 0
instToBinary m (MUL rd ri        rj) = argsToBinary 3  rd ri rj 0
instToBinary m (DIV rd ri        rj) = argsToBinary 4  rd ri rj 0
-- Logic
instToBinary m (AND rd ri        rj) = argsToBinary 5  rd ri rj 0
instToBinary m (OR  rd ri        rj) = argsToBinary 6  rd ri rj 0
instToBinary m (NOT rd ri          ) = argsToBinary 7  rd ri 0  0
-- Control Flow
instToBinary m (JMP            ri  ) = argsToBinary 8  0  ri 0  0
instToBinary m (BEZ ri (Left  c )  ) = argsToBinary 9  0  ri 0  c
instToBinary m (BEZ ri (Right l )  ) = argsToBinary 9  0  ri 0  (labelToConst m l)
-- Comparion
instToBinary m (CEQ rd ri        rj) = argsToBinary 10 rd ri rj 0
instToBinary m (CGT rd ri        rj) = argsToBinary 11 rd ri rj 0
-- Load/Store
instToBinary m (LDC rd (Left  c)   ) = argsToBinary 12 rd 0  0  c
instToBinary m (LDC rd (Right l)   ) = argsToBinary 12 rd 0  0  (labelToConst m l)
instToBinary m (LDM rd ri          ) = argsToBinary 13 rd ri 0  0
instToBinary m (STM ri           rj) = argsToBinary 14 0  ri rj 0
-- Misc
instToBinary m (NOP                ) = argsToBinary 0  0  0  0  0
instToBinary m (HALT               ) = argsToBinary 15 0  0  0  0

-- |This function concatenates all of the components of an instruction in binary
--  representation.
argsToBinary :: Word8 -> Register -> Register -> Register -> Constant -> [ Word8 ]
argsToBinary a d i j c = [   fromIntegral ((a .&. 0x0F) `shiftL` 4)
                         .|. fromIntegral  (d .&. 0x0F)
                         ,   fromIntegral ((i .&. 0x0F) `shiftL` 4)
                         .|. fromIntegral  (j .&. 0x0F)
                         ,   fromIntegral (c `shiftR` 8)
                         ,   fromIntegral  c
                         ]

-- |This function returns the physical address for @l@. If @l@ is not yet
--  defined, 'error' is called.
labelToConst :: LabelMap -> Label -> Constant
labelToConst m l = case Map.lookup l m of
                    Just w  -> w * 4
                    Nothing -> error $ "Undefined Label: " ++ show l ++ "."
