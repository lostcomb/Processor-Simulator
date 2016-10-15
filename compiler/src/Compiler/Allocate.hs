-- |This module re-allocates registers so that the program uses only
--  the number allowed by the architecture.

module Compiler.Allocate
  ( allocate
  ) where

import Assembly.Instruction

type Colour = Int

data Node = Node Register Colour
data Edge = Edge Node Node
data Graph = Graph [ Node ] [ Edge ]

allocate :: [ Instruction ] -> [ Instruction ]
allocate = undefined;
