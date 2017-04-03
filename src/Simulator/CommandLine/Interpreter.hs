{-
This file is part of aca-processor-simulator.

aca-processor-simulator is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

aca-processor-simulator is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with aca-processor-simulator.  If not, see <http://www.gnu.org/licenses/>.
-}
module Simulator.CommandLine.Interpreter
  ( interpret
  ) where

import Data.Map (toList)
import Data.Bits
import Data.List
import Data.Word
import Text.Printf
import System.Exit
import Control.Lens
import Control.Monad.State

import Simulator.Data.Processor
import Simulator.CommandLine.Command

interpret :: (Type -> ProcessorState ()) -> [ Command ] -> ProcessorState ()
interpret step cmds = mapM_ (interpret' step) cmds

interpret' :: (Type -> ProcessorState ()) -> Command -> ProcessorState ()
interpret' step (Step    i) = replicateM_ i $ condM (liftM not $ use halted)
                                  (use (options.procType) >>= step >>
                                     condM (use halted)
                                       (liftIO $ putStrLn "Execution Halted.")
                                       (return ()))
                                  (return ())
interpret' step (Continue ) = do whileM_ (liftM not $ use halted)
                                   $ use (options.procType) >>= step
                                 liftIO $ putStrLn "Execution Halted."
interpret' step (Registers ) = printRegisters
interpret' step (Memory    ) = printMemory
interpret' step (Stats     ) = do simData <- use $ simData
                                  liftIO . putStrLn . toString $ simData
                                  liftIO . putStrLn $  "Branch prediction rate (hits / predictions): "
                                                    ++ printf "%.2f" (branchPredictionRate simData)
                                  liftIO . putStrLn $  "Issue rate (instructions / cycle): "
                                                    ++ printf "%.1f" (issueRate simData)
                                  liftIO . putStrLn $  "Writeback rate (instructions / cycle): "
                                                    ++ printf "%.1f" (writebackRate simData)
interpret' step (FetchI    ) = do liftIO . putStrLn $ "Fetch Stage:"
                                  stall   <- use $ fetchStage.stalled
                                  liftIO . putStrLn $ "  Stalled: " ++ show stall
                                  pc      <- use $ fetchStage.programCounter
                                  liftIO . putStrLn $ "  PC: " ++ show pc
                                  output  <- use $ decInputLatches
                                  liftIO . putStrLn $ "  Output: " ++ show output
interpret' step (DecodeI   ) = do liftIO . putStrLn $ "Decode Stage:"
                                  stall   <- use $ decodeStage.stalled
                                  liftIO . putStrLn $ "  Stalled: " ++ show stall
                                  input  <- use $ decInputLatches
                                  liftIO . putStrLn $ "  Input: " ++ show input
                                  output  <- use $ issInputLatches
                                  liftIO . putStrLn $ "  Output: " ++ show output
interpret' step (ReOrderI  ) = do liftIO . putStrLn $ "ReOrder Stage:"
                                  stall   <- use $ robStage.stalled
                                  liftIO . putStrLn $ "  Stalled: " ++ show stall
interpret' step (IssueI    ) = do liftIO . putStrLn $ "Issue Stage:"
                                  stall   <- use $ issueStage.stalled
                                  liftIO . putStrLn $ "  Stalled: " ++ show stall
                                  input  <- use $ issInputLatches
                                  liftIO . putStrLn $ "  Input: " ++ show input
                                  output  <- use $ exeInputLatches
                                  liftIO . putStrLn $ "  Output: " ++ show output
interpret' step (ExecuteI  ) = do liftIO . putStrLn $ "Execute Stage:"
                                  stall   <- use $ executeStage.stalled
                                  liftIO . putStrLn $ "  Stalled: " ++ show stall
                                  bypass  <- use $ executeStage.bypassValues
                                  liftIO . putStrLn $ "  Bypass Values: " ++ show bypass
                                  sp      <- use $ executeStage.subPipeline
                                  liftIO . putStrLn $ "  Sub Pipeline: " ++ show sp
                                  input   <- use $ exeInputLatches
                                  liftIO . putStrLn $ "  Input: " ++ show input
                                  output  <- use $ wrbInputLatches
                                  liftIO . putStrLn $ "  Output: " ++ show output
interpret' step (WritebackI) = do liftIO . putStrLn $ "Writeback Stage:"
                                  stall   <- use $ writebackStage.stalled
                                  liftIO . putStrLn $ "  Stalled: " ++ show stall
                                  input  <- use $ wrbInputLatches
                                  liftIO . putStrLn $ "  Input: " ++ show input
interpret' step (Set inst c) = setInstCycles inst c
interpret' step (Get inst  ) = printInstCycles inst
interpret' step (Latches   ) = do pt  <- use $ options.procType
                                  dec <- use decInputLatches
                                  rob <- use robInputLatches
                                  iss <- use issInputLatches
                                  exe <- use exeInputLatches
                                  wrb <- use wrbInputLatches
                                  let arrow   = replicate 5 ' ' ++ "^"
                                      to_str (Left  l) = show l ++ "\n"
                                      to_str (Right r) = unlines . map show $ r
                                  writebackStalled <- use $ writebackStage.isStalled
                                  liftIO . putStrLn $ "WRB: " ++ show writebackStalled
                                  liftIO . putStr   . to_str $ wrb
                                  liftIO . putStrLn $ arrow
                                  executeStalled <- use $ executeStage.isStalled
                                  liftIO . putStrLn $ "EXE: " ++ show executeStalled
                                  liftIO . putStr   . to_str $ exe
                                  liftIO . putStrLn $ arrow
                                  if pt /= Superscalar then do
                                    issueStalled <- use $ issueStage.isStalled
                                    liftIO . putStrLn $ "ISS: " ++ show issueStalled
                                    liftIO . putStr   . to_str $ iss
                                    liftIO . putStrLn $ arrow
                                  else do
                                    robStalled <- use $ robStage.isStalled
                                    liftIO . putStrLn $ "ROB: " ++ show robStalled
                                    liftIO . putStrLn . unlines . map show . fst $ rob
                                    liftIO . putStr   . unlines . map show . snd $ rob
                                    liftIO . putStrLn $ arrow
                                  decodeStalled <- use $ decodeStage.isStalled
                                  liftIO . putStrLn $ "DEC: " ++ show decodeStalled
                                  liftIO . putStr   . to_str $ dec
                                  liftIO . putStrLn $ arrow
                                  fetchStalled <- use $ fetchStage.isStalled
                                  fetchHalted  <- use $ fetchStage.halt
                                  liftIO . putStrLn $ "FET: " ++ show fetchStalled ++ " halt: " ++ show fetchHalted
interpret' step (Caches    ) = do btac_cache <- use $ btac
                                  liftIO . putStrLn $ "BTAC:"
                                  liftIO . putStrLn . show $ btac_cache
                                  pattern_hist <- use $ patternHistory
                                  liftIO . putStrLn $ "Pattern History:"
                                  liftIO . putStrLn . show $ pattern_hist
                                  rat <- use $ registerAliasTable
                                  liftIO . putStrLn $ "Register Alias Table:"
                                  liftIO . putStrLn . show $ rat
interpret' step (ROB       ) = do rob <- use $ robStage.buffer
                                  liftIO . putStrLn $ "Reorder Buffer:"
                                  liftIO . putStrLn . unlines . map show $ rob
                                  liftIO . putStrLn $ "Reservation Stations:"
                                  rss <- use reservationStations
                                  liftIO . putStrLn . unlines . map show $ rss
interpret' step (Quit      ) = liftIO exitSuccess
interpret' step (ShowW     ) = liftIO . putStrLn $ warrantyNotice
interpret' step (ShowC     ) = do liftIO . putStrLn $ verbatimNotice
                                  liftIO . putStrLn $ ""
                                  liftIO . putStrLn $ modifiedNotice

warrantyNotice :: String
warrantyNotice =  "  THERE IS NO WARRANTY FOR THE PROGRAM, TO THE EXTENT PERMITTED BY\n"
               ++ "APPLICABLE LAW.  EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT\n"
               ++ "HOLDERS AND/OR OTHER PARTIES PROVIDE THE PROGRAM \"AS IS\" WITHOUT WARRANTY\n"
               ++ "OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO,\n"
               ++ "THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR\n"
               ++ "PURPOSE.  THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE PROGRAM\n"
               ++ "IS WITH YOU.  SHOULD THE PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF\n"
               ++ "ALL NECESSARY SERVICING, REPAIR OR CORRECTION."

verbatimNotice :: String
verbatimNotice =  "  You may convey verbatim copies of the Program's source code as you\n"
               ++ "receive it, in any medium, provided that you conspicuously and\n"
               ++ "appropriately publish on each copy an appropriate copyright notice;\n"
               ++ "keep intact all notices stating that this License and any\n"
               ++ "non-permissive terms added in accord with section 7 apply to the code;\n"
               ++ "keep intact all notices of the absence of any warranty; and give all\n"
               ++ "recipients a copy of this License along with the Program.\n\n"
               ++ "  You may charge any price or no price for each copy that you convey,\n"
               ++ "and you may offer support or warranty protection for a fee."

modifiedNotice :: String
modifiedNotice =  "  You may convey a work based on the Program, or the modifications to\n"
               ++ "produce it from the Program, in the form of source code under the\n"
               ++ "terms of section 4, provided that you also meet all of these conditions:\n\n"
               ++ "    a) The work must carry prominent notices stating that you modified\n"
               ++ "    it, and giving a relevant date.\n\n"
               ++ "    b) The work must carry prominent notices stating that it is\n"
               ++ "    released under this License and any conditions added under section\n"
               ++ "    7.  This requirement modifies the requirement in section 4 to\n"
               ++ "    \"keep intact all notices\".\n\n"
               ++ "    c) You must license the entire work, as a whole, under this\n"
               ++ "    License to anyone who comes into possession of a copy.  This\n"
               ++ "    License will therefore apply, along with any applicable section 7\n"
               ++ "    additional terms, to the whole of the work, and all its parts,\n"
               ++ "    regardless of how they are packaged.  This License gives no\n"
               ++ "    permission to license the work in any other way, but it does not\n"
               ++ "    invalidate such permission if you have separately received it.\n\n"
               ++ "    d) If the work has interactive user interfaces, each must display\n"
               ++ "    Appropriate Legal Notices; however, if the Program has interactive\n"
               ++ "    interfaces that do not display Appropriate Legal Notices, your\n"
               ++ "    work need not make them do so.\n\n"
               ++ "  A compilation of a covered work with other separate and independent\n"
               ++ "works, which are not by their nature extensions of the covered work,\n"
               ++ "and which are not combined with it such as to form a larger program,\n"
               ++ "in or on a volume of a storage or distribution medium, is called an\n"
               ++ "\"aggregate\" if the compilation and its resulting copyright are not\n"
               ++ "used to limit the access or legal rights of the compilation's users\n"
               ++ "beyond what the individual works permit.  Inclusion of a covered work\n"
               ++ "in an aggregate does not cause this License to apply to the other\n"
               ++ "parts of the aggregate."

setInstCycles :: String -> Int -> ProcessorState ()
setInstCycles inst c
  = do func <- use $ instCycles
       instCycles .= case inst of
         "nop"  -> (\i -> if isNop  i then c else func i)
         "add"  -> (\i -> if isAdd  i then c else func i)
         "sub"  -> (\i -> if isSub  i then c else func i)
         "mul"  -> (\i -> if isMul  i then c else func i)
         "div"  -> (\i -> if isDiv  i then c else func i)
         "and"  -> (\i -> if isAnd  i then c else func i)
         "or"   -> (\i -> if isOr   i then c else func i)
         "not"  -> (\i -> if isNot  i then c else func i)
         "jmp"  -> (\i -> if isJmp  i then c else func i)
         "bez"  -> (\i -> if isBez  i then c else func i)
         "ceq"  -> (\i -> if isCeq  i then c else func i)
         "cgt"  -> (\i -> if isCgt  i then c else func i)
         "ldc"  -> (\i -> if isLdc  i then c else func i)
         "ldm"  -> (\i -> if isLdm  i then c else func i)
         "stm"  -> (\i -> if isStm  i then c else func i)
         "halt" -> (\i -> if isHalt i then c else func i)
         _      -> func

printInstCycles :: String -> ProcessorState ()
printInstCycles inst
  = do func <- use $ instCycles
       liftIO . putStrLn $ case inst of
         "nop"  -> show . func $ Nop
         "add"  -> show . func $ Add R0 R0 R0
         "sub"  -> show . func $ Sub R0 R0 R0
         "mul"  -> show . func $ Mul R0 R0 R0
         "div"  -> show . func $ Div R0 R0 R0
         "and"  -> show . func $ And R0 R0 R0
         "or"   -> show . func $ Or  R0 R0 R0
         "not"  -> show . func $ Not R0 R0
         "jmp"  -> show . func $ Jmp R0
         "bez"  -> show . func $ Bez R0 0
         "ceq"  -> show . func $ Ceq R0 R0 R0
         "cgt"  -> show . func $ Cgt R0 R0 R0
         "ldc"  -> show . func $ Ldc R0 0
         "ldm"  -> show . func $ Ldm R0 R0
         "stm"  -> show . func $ Stm R0 R0
         "halt" -> show . func $ Halt
         _      -> "Invalid instrucion: " ++ inst

printRegisters :: ProcessorState ()
printRegisters = do let groups = chunksOf 4 [(minBound::Register)..]
                        section = replicate 4 . replicate 18 $ '-'
                        line = "+" ++ intercalate "+" section ++ "+"
                    liftIO $ putStrLn line
                    mapM_ (\rs -> do mapM_ (\r -> do liftIO $ putStr "|"
                                                     genReg r) rs
                                     liftIO $ putStrLn "|") groups
                    liftIO $ putStrLn line
  where genReg :: Register -> ProcessorState ()
        genReg r = do val <- use $ regFile.regVal r
                      flag <- use $ regFile.regFlag r
                      let r_s = show r ++ ": "
                          v_s = show val ++ " " ++ flagToString flag
                          padding = replicate (16 - length r_s - length v_s) ' '
                      liftIO $ putStr $ " " ++ r_s ++ padding ++ v_s ++ " "
        flagToString :: Flag -> String
        flagToString c
          | c == 0    = "c"
          | c >  0    = "d"
          | otherwise = "u"

printMemory :: ProcessorState ()
printMemory = do mem <- use $ dataMem
                 let groups = chunksOf 3 . chunksOf 4 $ toList mem
                     section = replicate 3 . replicate 23 $ '-'
                     line = "+" ++ intercalate "+" section ++ "+"
                 liftIO $ putStrLn line
                 mapM_ (\ms -> do let str = concat . map (\m -> "|" ++ genMem m) $ ms
                                      padding = replicate (3 * 23 + 3 - length str) ' '
                                  liftIO $ putStrLn $ str ++ padding ++ "|") groups
                 liftIO $ putStrLn line
  where genMem :: [ (Word32, Word8) ] -> String
        genMem (b1:b2:b3:b4:[]) = " " ++ i_s ++ padding ++ v_s ++ " "
          where v1 = fromIntegral (snd b1) `shiftL` 24
                v2 = fromIntegral (snd b2) `shiftL` 16
                v3 = fromIntegral (snd b3) `shiftL` 8
                v4 = fromIntegral (snd b4)
                i_s = show $ fst b1
                val = (v1 .|. v2 .|. v3 .|. v4) :: Word32
                v_s = show val
                padding = replicate (21 - length i_s - length v_s) ' '
        genMem _                = ""

chunksOf :: Int -> [ a ] -> [ [ a ] ]
chunksOf n xs
  | length xs <= n = [ xs ]
  | otherwise      = (take n xs) : chunksOf n (drop n xs)
