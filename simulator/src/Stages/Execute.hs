module Stages.Execute
  ( execute
  ) where

import Data.Bits
import Components.Registers
import Components.Instructions
import Components.ProcessorState

execute :: Instruction Int -> ProcessorState -> ProcessorState
execute i s = incExecutedInsts $ execute' i s

execute' :: Instruction Int -> ProcessorState -> ProcessorState
execute' (Nop x) s = updateCycles x s

execute' (Add x (Reg rd) (Reg ri) (Reg rj)) s = updateCycles x . setReg rd (vi + vj) $ s
  where vi = getReg ri s
        vj = getReg rj s

execute' (Sub x (Reg rd) (Reg ri) (Reg rj)) s = updateCycles x . setReg rd (vi - vj) $ s
  where vi = getReg ri s
        vj = getReg rj s

execute' (Mul x (Reg rd) (Reg ri) (Reg rj)) s = updateCycles x . setReg rd (vi * vj) $ s
  where vi = getReg ri s
        vj = getReg rj s

execute' (Div x (Reg rd) (Reg ri) (Reg rj)) s = updateCycles x . setReg rd (vi `div` vj) $ s
  where vi = getReg ri s
        vj = getReg rj s

execute' (And x (Reg rd) (Reg ri) (Reg rj)) s = updateCycles x . setReg rd (vi .&. vj) $ s
  where vi = getReg ri s
        vj = getReg rj s

execute' (Or  x (Reg rd) (Reg ri) (Reg rj)) s = updateCycles x . setReg rd (vi .|. vj) $ s
  where vi = getReg ri s
        vj = getReg rj s

execute' (Not x (Reg rd) (Reg ri)) s = updateCycles x . setReg rd (if vi == 0
                                                                     then 1
                                                                     else 0) $ s
  where vi = getReg ri s

execute' (Jmp x (Reg ri)) s = updateCycles x . setReg pc vi $ s
  where vi = getReg ri s

execute' (Bez x (Reg ri) c) s = updateCycles x (if vi == 0
                                                  then setReg pc (fromIntegral c) s
                                                  else s)
  where vi = getReg ri s

execute' (Ceq x (Reg rd) (Reg ri) (Reg rj)) s = updateCycles x . setReg rd (if vi == vj
                                                                              then 1
                                                                              else 0) $ s
  where vi = getReg ri s
        vj = getReg rj s

execute' (Cgt x (Reg rd) (Reg ri) (Reg rj)) s = updateCycles x . setReg rd (if vi > vj
                                                                              then 1
                                                                              else 0) $ s
  where vi = getReg ri s
        vj = getReg rj s

execute' (Ldc x (Reg rd) c) s = updateCycles x . setReg rd (fromIntegral c) $ s

execute' (Ldm x (Reg rd) (Reg ri)) s = updateCycles x . setReg rd mem $ s
  where vi  = getReg ri s
        mem = getMemory (fromIntegral vi) s

execute' (Stm x (Reg ri) (Reg rj)) s = updateCycles x . setMemory (fromIntegral vi) vj $ s
  where vi = getReg ri s
        vj = getReg rj s

execute' (Halt) s = haltExecution s
