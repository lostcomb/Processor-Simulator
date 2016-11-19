module Simulator.CommandLine.Command
  ( Command(..)
  ) where

data Command = Step Int
             | Continue
             | Registers
             | Memory
             | Stats
             | DecodeI
             | IssueI
             | ExecuteI
             | WritebackI
             | Set String Int
             | Get String
             | Quit
             deriving (Show, Eq, Read)
