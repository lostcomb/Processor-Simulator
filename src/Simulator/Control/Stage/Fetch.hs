module Control.Stage.Fetch
  ( fetch
  ) where

import Data.Word
import Data.Stage
import Data.Processor
import Control.Monad.State

fetch :: (Processor p, Fetch p) => State p [ Maybe (Word8, Word8, Word8, Word8) ]
fetch = do n_insts <- getNoOfInsts
           f <- getFetch
           if stalled f
             then do output <- getDecInputLatches
                     return output
             else mapM (\_ -> do pc_val <- getProgramCounter
                                 i1 <- getInstMemItem (fromIntegral pc_val)
                                 i2 <- getInstMemItem (fromIntegral (pc_val + 1))
                                 i3 <- getInstMemItem (fromIntegral (pc_val + 2))
                                 i4 <- getInstMemItem (fromIntegral (pc_val + 3))
                                 setProgramCounter (pc_val + 4)
                                 return $ Just (i1, i2, i3, i4)) [1..n_insts]
