module Main where

import Assembler
import Assembler.Parser
import Assembler.Instruction

import Prelude hiding (writeFile)
import qualified Prelude as P

import Data.Word
import Data.Bits
import System.Environment
import Data.ByteString.Lazy (writeFile, pack)

main :: IO ()
main = do args <- getArgs
          case args of
            [ "assemble"    , r_p, w_p ] -> do c <- readFile r_p
                                               P.writeFile w_p $ show $ assemble . parseAssembly $ c
            [ "assemble_bin", r_p, w_p ] -> do c <- readFile r_p
                                               let insts = assembleBinary . parseAssembly $ c
                                               writeFile w_p $ pack $ concat . map toWord8 $ insts
            _                            -> error $  "Usage: (assemble | assemble_bin) "
                                                  ++ "read_path.asm write_path.o"

toWord8 :: Word32 -> [ Word8 ]
toWord8 w = [ fromIntegral (w `shiftR` 24)
            , fromIntegral (w `shiftR` 16)
            , fromIntegral (w `shiftR` 8)
            , fromIntegral  w
            ]
