module Simulator.Simulator
  ( runProcessor
  ) where

import Data.Map (toList)
import Data.Bits
import Data.List
import Data.Word
import System.IO
import System.Exit
import Control.Lens
import Control.Monad
import Control.Monad.State

import Simulator.CmdParser
import Simulator.Data.Processor
import Simulator.Control.Stage.Fetch
import Simulator.Control.Stage.Decode
import Simulator.Control.Stage.Issue
import Simulator.Control.Stage.Execute
import Simulator.Control.Stage.Writeback

runProcessor :: StateT [ Command ] (StateT Processor IO) a
runProcessor = do liftIO $ putStrLn "Processor Simulator (c) Julian Loscombe 2016\n"
                  forever $ do liftIO $ putStr "*>"
                               liftIO $ hFlush stdout
                               command_str <- liftIO $ getLine
                               case parse command_str of
                                 (Left   e) -> do put []
                                                  liftIO $ putStrLn $ show e
                                 (Right []) -> do cs <- get
                                                  lift $ performCommands cs
                                 (Right cs) -> do put cs
                                                  lift $ performCommands cs

step :: Type -> ProcessorState ()
step Scalar      = scalarProcessor
step Pipelined   = pipelinedProcessor
step Superscalar = superscalarProcessor

scalarProcessor :: ProcessorState ()
scalarProcessor
  = do f <- scalarFetch
       d <- scalarDecode f
       i <- scalarIssue d
       e <- scalarExecute i
       w <- scalarWriteback e
       simData.cycles += 5

pipelinedProcessor :: ProcessorState ()
pipelinedProcessor
  = do f <- pipelinedFetch
       d <- pipelinedDecode f
       i <- pipelinedIssue d
       e <- pipelinedExecute i
       w <- pipelinedWriteback e
       decInputLatches .= f
       issInputLatches .= d
       exeInputLatches .= i
       wrbInputLatches .= e
       simData.cycles += 1

superscalarProcessor :: ProcessorState ()
superscalarProcessor = undefined

performCommands :: [ Command ] -> ProcessorState ()
performCommands cmds = mapM_ performCommand cmds

performCommand :: Command -> ProcessorState ()
performCommand (Step i)     = replicateM_ i $ use (options.procType) >>= step
performCommand (Continue)   = do whileM_ (liftM not $ use halted)
                                   $ use (options.procType) >>= step
                                 liftIO $ putStrLn "Execution Halted."
performCommand (Registers)  = printRegisters
performCommand (Memory)     = printMemory
performCommand (Stats)      = printStatistics
performCommand (DecodeI)    = do dec <- use $ decInputLatches
                                 liftIO $ putStrLn $ show dec
performCommand (IssueI)     = do iss <- use $ issInputLatches
                                 liftIO $ putStrLn $ show iss
performCommand (ExecuteI)   = do exe <- use $ exeInputLatches
                                 liftIO $ putStrLn $ show exe
performCommand (WritebackI) = do wrb <- use $ wrbInputLatches
                                 liftIO $ putStrLn $ show wrb
performCommand (Quit)       = liftIO exitSuccess

printRegisters :: ProcessorState ()
printRegisters = do let groups = chunksOf 4 [(minBound::Register)..]
                        section = replicate 4 . replicate 17 $ '-'
                        line = "+" ++ intercalate "+" section ++ "+"
                    liftIO $ putStrLn line
                    mapM_ (\rs -> do mapM_ (\r -> do liftIO $ putStr "|"
                                                     genReg r) rs
                                     liftIO $ putStrLn "|") groups
                    liftIO $ putStrLn line
  where genReg :: Register -> ProcessorState ()
        genReg r = do val <- use $ regFile.regVal r
                      let r_s = show r ++ ": "
                          v_s = show val
                          padding = replicate (15 - length r_s - length v_s) ' '
                      liftIO $ putStr $ " " ++ r_s ++ padding ++ v_s ++ " "

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

printStatistics :: ProcessorState ()
printStatistics = do cs <- use $ simData.cycles
                     is <- use $ simData.insts
                     f <- use $ simData.fetchStalledCount
                     d <- use $ simData.decodeStalledCount
                     i <- use $ simData.issueStalledCount
                     e <- use $ simData.executeStalledCount
                     w <- use $ simData.writebackStalledCount
                     liftIO $ putStrLn $ "Cycles: "                  ++ show cs
                     liftIO $ putStrLn $ "Instructions: "            ++ show is
                     liftIO $ putStrLn $ "Fetch stalled count: "     ++ show f
                     liftIO $ putStrLn $ "Decode stalled count: "    ++ show d
                     liftIO $ putStrLn $ "Issue stalled count: "     ++ show i
                     liftIO $ putStrLn $ "Execute stalled count: "   ++ show e
                     liftIO $ putStrLn $ "Writeback stalled count: " ++ show w


chunksOf :: Int -> [ a ] -> [ [ a ] ]
chunksOf n xs
  | length xs <= n = [ xs ]
  | otherwise      = (take n xs) : chunksOf n (drop n xs)
