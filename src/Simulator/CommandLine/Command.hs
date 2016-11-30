module Simulator.CommandLine.Command
  ( Command(..)
  ) where

data Command = Step Int
             | Continue
             | Registers
             | Memory
             | Stats
             | FetchI
             | DecodeI
             | IssueI
             | ExecuteI
             | WritebackI
             | Set String Int
             | Get String
             | Latches
             | Caches
             | Quit
             deriving (Show, Eq, Read)
