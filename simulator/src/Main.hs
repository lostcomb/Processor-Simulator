module Main
  ( main
  ) where

import Prelude hiding (readFile)
import System.Exit
import System.Environment
import Data.Int
import Data.Char
import Data.Word
import Stage.Fetch
import Stage.Decode
import Stage.Execute
import Data.List
import Data.List.Split
import Data.ByteString.Lazy (readFile, unpack)
import Component.Register
import Component.RegisterFile
import Component.Processor
import Control.Monad.State

main :: IO ()
main = do (program:_) <- getArgs
          contents <- readFile program
          exec $ newProcessor $ unpack contents

exec :: Processor -> IO ()
exec p = do let str = evalState displayProcessor p
                halted = evalState isHalted p
            putStrLn str
            if halted
              then exitSuccess
              else do getChar
                      let (inst, p') = runState step p
                      putStrLn inst
                      exec p'

displayProcessor :: State Processor String
displayProcessor
  = do reg_lines <- generateRegisterLines
       mem_lines <- generateMemoryLines
       executed_insts <- getExecutedInsts
       cycles <- getCycles
       let horz_section = replicate 4 . replicate 17 $ '-'
           reg_line = "\n+" ++ intercalate "+" horz_section ++ "+\n"
           mem_line = "\n+" ++ intercalate "-" horz_section ++ "+\n"
           reg_str = intercalate reg_line reg_lines
           mem_str = intercalate ("\n|" ++ replicate 71 ' ' ++ "|\n") mem_lines
       return $  "Registers:" ++ reg_line ++ reg_str ++ reg_line
              ++ "Memory:"    ++ mem_line ++ mem_str ++ mem_line
              ++ "Stats:\n\tExecuted " ++ show executed_insts
              ++ " instructions after " ++ show cycles ++ " cycles."

generateMemoryLines :: State Processor [ String ]
generateMemoryLines
  = do mem <- getMemoryContents
       let mem_groups = chunksOf 3 mem
           gen_mem = \m -> map (uncurry generateMemory) m
       return $ map (\x -> "|" ++ intercalate " " (gen_mem x) ++ "|") mem_groups

generateMemory :: Word32 -> Int32 -> String
generateMemory i v = " " ++ i_s ++ padding ++ v_s ++ " "
  where i_s     = show i ++ ": "
        v_s     = show v
        padding = replicate (21 - (length i_s + length v_s)) ' '

generateRegisterLines :: State Processor [ String ]
generateRegisterLines
  = do reg_file <- getRegisterFile
       let regs = getRegisters reg_file
           reg_groups = chunksOf 4 regs
           gen_reg = \r -> map (uncurry generateRegister) r
       return $ map (\x -> "|" ++ intercalate "|" (gen_reg x) ++ "|") reg_groups

generateRegister :: Register -> Int32 -> String
generateRegister r v = " " ++ r_s ++ padding ++ v_s ++ " "
  where r_s     = show r ++ ": "
        v_s     = show v
        padding = replicate (15 - (length r_s + length v_s)) ' '

step :: State Processor String
step = do bi <- fetch
          i <- decode bi
          execute i
          return $ "Executing Instruction: " ++ show i
