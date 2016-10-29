module Compiler.Allocator
  ( allocate
  ) where

import Compiler.Instruction

type Colour = Int

data Node = Node Register Colour
data Edge = Edge Node Node
data Graph = Graph [ Node ] [ Edge ]

allocate :: [ Instruction ] -> [ Instruction ]
allocate = undefined;
