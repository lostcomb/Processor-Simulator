module Stages.Execute
  ( execute
  ) where

import Data.Bits
import Components.Registers
import Components.Instructions
import Components.Processor
import Control.Monad.State

execute :: Instruction Int -> State Processor ()
execute i = do incExecutedInsts
               execute' i

execute' :: Instruction Int -> State Processor ()
execute' (Nop x) = updateCycles x

execute' (Add x (Reg rd) (Reg ri) (Reg rj))
  = do updateCycles x
       vi <- getReg ri
       vj <- getReg rj
       setReg rd (vi + vj)

execute' (Sub x (Reg rd) (Reg ri) (Reg rj))
  = do updateCycles x
       vi <- getReg ri
       vj <- getReg rj
       setReg rd (vi - vj)

execute' (Mul x (Reg rd) (Reg ri) (Reg rj))
  = do updateCycles x
       vi <- getReg ri
       vj <- getReg rj
       setReg rd (vi * vj)

execute' (Div x (Reg rd) (Reg ri) (Reg rj))
  = do updateCycles x
       vi <- getReg ri
       vj <- getReg rj
       setReg rd (vi `div` vj)

execute' (And x (Reg rd) (Reg ri) (Reg rj))
  = do updateCycles x
       vi <- getReg ri
       vj <- getReg rj
       setReg rd (vi .&. vj)

execute' (Or  x (Reg rd) (Reg ri) (Reg rj))
  = do updateCycles x
       vi <- getReg ri
       vj <- getReg rj
       setReg rd (vi .|. vj)

execute' (Not x (Reg rd) (Reg ri))
  = do updateCycles x
       vi <- getReg ri
       setReg rd (complement vi)

execute' (Jmp x (Reg ri))
  = do updateCycles x
       vi <- getReg ri
       setReg pc vi

execute' (Bez x (Reg ri) c)
  = do updateCycles x
       vi <- getReg ri
       if vi == 0
         then setReg pc (fromIntegral c)
         else return ()

execute' (Ceq x (Reg rd) (Reg ri) (Reg rj))
  = do updateCycles x
       vi <- getReg ri
       vj <- getReg rj
       let res = if vi == vj then 1 else 0
       setReg rd res

execute' (Cgt x (Reg rd) (Reg ri) (Reg rj))
  = do updateCycles x
       vi <- getReg ri
       vj <- getReg rj
       let res = if vi > vj then 1 else 0
       setReg rd res

execute' (Ldc x (Reg rd) c)
  = do updateCycles x
       setReg rd (fromIntegral c)

execute' (Ldm x (Reg rd) (Reg ri))
  = do updateCycles x
       vi <- getReg ri
       mem <- getMemory (fromIntegral vi)
       setReg rd mem

execute' (Stm x (Reg ri) (Reg rj))
  = do updateCycles x
       vi <- getReg ri
       vj <- getReg rj
       setMemory (fromIntegral vi) vj

execute' (Halt) = haltExecution
