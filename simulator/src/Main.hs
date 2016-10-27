module Main
  ( main
  ) where

import Stages.Fetch
import Stages.Decode
import Stages.Execute
import Components.ProcessorState

main :: IO ()
main = undefined --TODO

step :: ProcessorState -> ProcessorState
step = uncurry execute . uncurry decode . fetch
