module Main
  ( main
  ) where

import Prelude hiding (readFile)
import System.Exit
import System.Environment
import Data.Char
import Data.Word
import Stages.Fetch
import Stages.Decode
import Stages.Execute
import Data.List
import Data.List.Split
import Data.ByteString.Lazy (readFile, unpack)
import Components.Registers
import Components.RegisterFile
import Components.ProcessorState

main :: IO ()
main = do (program:_) <- getArgs
          contents <- readFile program
          exec $ newProcessorState $ unpack contents

exec :: ProcessorState -> IO ()
exec s = do displayProcessorState s
            if isHalted s
              then exitSuccess
              else do getChar
                      s' <- step s
                      exec s'

displayProcessorState :: ProcessorState -> IO ()
displayProcessorState s =
  do let horz_section = replicate 4 . replicate 17 $ '-'
         top_line     = "+" ++ intercalate "+" horz_section ++ "+"
         middle_line  = "\n+" ++ intercalate "+" horz_section ++ "+\n"
         bottom_line  = "+" ++ intercalate "+" horz_section ++ "+"
         reg_lines    = intercalate middle_line $ generateRegisterLines s
         mem_lines    = intercalate ("\n|" ++ replicate 71 ' ' ++ "|\n") $ generateMemoryLines s
     putStrLn "Registers:"
     putStrLn top_line
     putStrLn reg_lines
     putStrLn bottom_line
     putStrLn "Memory:"
     putStrLn $ "+" ++ replicate 71 '-' ++ "+"
     putStrLn mem_lines
     putStrLn $ "+" ++ replicate 71 '-' ++ "+"
     putStrLn "Stats:"
     putStrLn $  "\tExecuted "
              ++ show (getExecutedInsts s)
              ++ " instructions after "
              ++ show (getCycles s)
              ++ " cycles."


generateMemoryLines :: ProcessorState -> [ String ]
generateMemoryLines s = mem_lines
  where mem_groups = chunksOf 3 $ getMemoryContents s
        mem_lines  = map (\x -> "|"
                             ++ intercalate " "
                                (map (uncurry generateMemory) x)
                             ++ "|") mem_groups

generateMemory :: Word32 -> Word32 -> String
generateMemory i v = " " ++ i_s ++ padding ++ v_s ++ " "
  where i_s     = show i ++ ": "
        v_s     = show v
        padding = replicate (21 - (length i_s + length v_s)) ' '

generateRegisterLines :: ProcessorState -> [ String ]
generateRegisterLines s = register_lines
  where register_groups = chunksOf 4 $ getRegisters (getRegisterFile s)
        register_lines  = map (\x -> "|"
                                  ++ intercalate "|"
                                     (map (uncurry generateRegister) x)
                                  ++ "|") register_groups

generateRegister :: RegisterName -> Word32 -> String
generateRegister r v = " " ++ r_s ++ padding ++ v_s ++ " "
  where r_s     = show r ++ ": "
        v_s     = show v
        padding = replicate (15 - (length r_s + length v_s)) ' '

step :: ProcessorState -> IO ProcessorState
step s = do let s'     = uncurry execute . uncurry decode . fetch $ s
                (i, _) = uncurry decode . fetch $ s
            putStrLn $ "Executing Instruction: " ++ show i
            return s'
