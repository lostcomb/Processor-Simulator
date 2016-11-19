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
performCommand (Step    i) = replicateM_ i $ use (options.procType) >>= step
performCommand (Continue ) = do whileM_ (liftM not $ use halted)
                                  $ use (options.procType) >>= step
                                liftIO $ putStrLn "Execution Halted."
performCommand (Registers ) = printRegisters
performCommand (Memory    ) = printMemory
performCommand (Stats     ) = printStatistics
performCommand (DecodeI   ) = do dec <- use $ decInputLatches
                                 liftIO $ putStrLn $ show dec
performCommand (IssueI    ) = do iss <- use $ issInputLatches
                                 liftIO $ putStrLn $ show iss
performCommand (ExecuteI  ) = do exe <- use $ exeInputLatches
                                 liftIO $ putStrLn $ show exe
performCommand (WritebackI) = do wrb <- use $ wrbInputLatches
                                 liftIO $ putStrLn $ show wrb
performCommand (Set inst c) = setInstCycles inst c
performCommand (Get inst  ) = printInstCycles inst
performCommand (Quit      ) = liftIO exitSuccess

setInstCycles :: String -> Int -> ProcessorState ()
setInstCycles "nop" c = do func <- use $ instCycles
                           instCycles .= (\i -> case i of
                                                  (Nop) -> c
                                                  _     -> func i)
setInstCycles "add" c = do func <- use $ instCycles
                           instCycles .= (\i -> case i of
                                                  (Add _ _ _) -> c
                                                  _           -> func i)
setInstCycles "sub" c = do func <- use $ instCycles
                           instCycles .= (\i -> case i of
                                                  (Sub _ _ _) -> c
                                                  _           -> func i)
setInstCycles "mul" c = do func <- use $ instCycles
                           instCycles .= (\i -> case i of
                                                  (Mul _ _ _) -> c
                                                  _           -> func i)
setInstCycles "div" c = do func <- use $ instCycles
                           instCycles .= (\i -> case i of
                                                  (Div _ _ _) -> c
                                                  _           -> func i)
setInstCycles "and" c = do func <- use $ instCycles
                           instCycles .= (\i -> case i of
                                                  (And _ _ _) -> c
                                                  _           -> func i)
setInstCycles "or"  c = do func <- use $ instCycles
                           instCycles .= (\i -> case i of
                                                  (Or  _ _ _) -> c
                                                  _           -> func i)
setInstCycles "not" c = do func <- use $ instCycles
                           instCycles .= (\i -> case i of
                                                  (Not _ _) -> c
                                                  _         -> func i)
setInstCycles "jmp" c = do func <- use $ instCycles
                           instCycles .= (\i -> case i of
                                                  (Jmp _) -> c
                                                  _       -> func i)
setInstCycles "bez" c = do func <- use $ instCycles
                           instCycles .= (\i -> case i of
                                                  (Bez _ _) -> c
                                                  _         -> func i)
setInstCycles "ceq" c = do func <- use $ instCycles
                           instCycles .= (\i -> case i of
                                                  (Ceq _ _ _) -> c
                                                  _           -> func i)
setInstCycles "cgt" c = do func <- use $ instCycles
                           instCycles .= (\i -> case i of
                                                  (Cgt _ _ _) -> c
                                                  _           -> func i)
setInstCycles "ldc" c = do func <- use $ instCycles
                           instCycles .= (\i -> case i of
                                                  (Ldc _ _) -> c
                                                  _         -> func i)
setInstCycles "ldm" c = do func <- use $ instCycles
                           instCycles .= (\i -> case i of
                                                  (Ldm _ _) -> c
                                                  _         -> func i)
setInstCycles "stm" c = do func <- use $ instCycles
                           instCycles .= (\i -> case i of
                                                  (Stm _ _) -> c
                                                  _         -> func i)
setInstCycles "halt" c = do func <- use $ instCycles
                            instCycles .= (\i -> case i of
                                                   (Halt) -> c
                                                   _      -> func i)
setInstCycles i _ = liftIO $ putStrLn $ "Invalid instruction: " ++ i

printInstCycles :: String -> ProcessorState ()
printInstCycles "nop"  = do cs <- use $ instCycles
                            liftIO $ putStrLn $ show $ cs $ Nop
printInstCycles "add"  = do cs <- use $ instCycles
                            liftIO $ putStrLn $ show $ cs $ Add R0 R0 R0
printInstCycles "sub"  = do cs <- use $ instCycles
                            liftIO $ putStrLn $ show $ cs $ Sub R0 R0 R0
printInstCycles "mul"  = do cs <- use $ instCycles
                            liftIO $ putStrLn $ show $ cs $ Mul R0 R0 R0
printInstCycles "div"  = do cs <- use $ instCycles
                            liftIO $ putStrLn $ show $ cs $ Div R0 R0 R0
printInstCycles "and"  = do cs <- use $ instCycles
                            liftIO $ putStrLn $ show $ cs $ And R0 R0 R0
printInstCycles "or"   = do cs <- use $ instCycles
                            liftIO $ putStrLn $ show $ cs $ Or  R0 R0 R0
printInstCycles "not"  = do cs <- use $ instCycles
                            liftIO $ putStrLn $ show $ cs $ Not R0 R0
printInstCycles "jmp"  = do cs <- use $ instCycles
                            liftIO $ putStrLn $ show $ cs $ Jmp R0
printInstCycles "bez"  = do cs <- use $ instCycles
                            liftIO $ putStrLn $ show $ cs $ Bez R0 0
printInstCycles "ceq"  = do cs <- use $ instCycles
                            liftIO $ putStrLn $ show $ cs $ Ceq R0 R0 R0
printInstCycles "cgt"  = do cs <- use $ instCycles
                            liftIO $ putStrLn $ show $ cs $ Cgt R0 R0 R0
printInstCycles "ldc"  = do cs <- use $ instCycles
                            liftIO $ putStrLn $ show $ cs $ Ldc R0 0
printInstCycles "ldm"  = do cs <- use $ instCycles
                            liftIO $ putStrLn $ show $ cs $ Ldm R0 R0
printInstCycles "stm"  = do cs <- use $ instCycles
                            liftIO $ putStrLn $ show $ cs $ Stm R0 R0
printInstCycles "halt" = do cs <- use $ instCycles
                            liftIO $ putStrLn $ show $ cs $ Halt
printInstCycles i      = liftIO $ putStrLn $ "Invalid instruction: " ++ i

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
