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
                                  liftIO . putStrLn $  "Issue rate (instructions / cycle): "
                                                    ++ printf "%.1f" (issueRate simData)
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
                                  spec    <- use $ decodeStage.speculative
                                  liftIO . putStrLn $ "  Speculative: " ++ show spec
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
                                  input  <- use $ exeInputLatches
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
interpret' step (ROB       ) = do rob <- use $ robStage.buffer
                                  liftIO . putStrLn $ "Reorder Buffer:"
                                  liftIO . putStrLn . unlines . map show $ rob
                                  liftIO . putStrLn $ "Reservation Stations:"
                                  rss <- use reservationStations
                                  liftIO . putStrLn . unlines . map show $ rss
interpret' step (Quit      ) = liftIO exitSuccess

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
