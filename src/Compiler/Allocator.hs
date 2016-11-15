-- |This module defines a graph colouring algorithm to reduce the number of
--  registers used.

module Compiler.Allocator
  ( allocate
  ) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.List
import Data.Maybe
import Control.Monad
import Control.Monad.State

import Compiler.Instruction

-- |These are the types used by the liveness analysis and graph colouring
--  algorithm.
type Colour     = Int
type In         = Map Instruction [ Register ]
type Out        = Map Instruction [ Register ]
type Use        = Map Instruction [ Register ]
type Def        = Map Instruction [ Register ]
type Labels     = Map Label Instruction
type Successors = Map Instruction [ Instruction ]

-- |These data types are used to build an interference graph.
data Node  = Node Register Colour
  deriving (Show, Read)
data Edge  = Edge Node Node
  deriving (Show, Eq, Read)
data Graph = Graph [ Node ] [ Edge ]
  deriving (Show, Eq, Read)

-- |This definition of Eq is so that comparison of nodes is agnostic of the
--  nodes colour. This is useful as the node colour changes during the graph
--  colouring algorithm, but the edges to be added back in have not had their
--  colours updated.
instance Eq Node where
  (Node n1 _) == (Node n2 _) = n1 == n2

instance Ord Node where
  compare (Node _ c1) (Node _ c2) = compare c1 c2

-- |This data type stores the state required to build and colour an
--  interference graph.
data Allocator = Allocator
  { labels     :: Labels
  , successors :: Successors
  , inSet      :: In
  , outSet     :: Out
  , useSet     :: Use
  , defSet     :: Def
  , graph      :: Graph
  }

-- |This function returns an empty allocator.
newAllocator :: Allocator
newAllocator = Allocator
  { labels     = Map.empty
  , successors = Map.empty
  , inSet      = Map.empty
  , outSet     = Map.empty
  , useSet     = Map.empty
  , defSet     = Map.empty
  , graph      = Graph [] []
  }

-- | This function performs register allocation on the specified program.
allocate :: [ Instruction ] -> [ Instruction ]
allocate prog = evalState (allocate' prog) newAllocator

-- |This function performs register allocation on the specified program.
allocate' :: [ Instruction ] -> State Allocator [ Instruction ]
allocate' prog = do buildLabelMap prog
                    buildUseDef prog
                    buildSuccessorSet prog
                    analyseLiveRegisters prog
                    buildInterferenceGraph prog
                    -- Colour the graph.
                    let loop k = do colourGraph k
                                    max_col <- getMaxColour
                                    if max_col < k
                                      then loop (k - 1)
                                      else return ()
                    k <- size
                    loop k
                    prog' <- updateRegisters prog
                    return prog'

-- |This function updates the registers in the instructions with the newly
--  allocated registers.
updateRegisters :: [ Instruction ] -> State Allocator [ Instruction ]
updateRegisters prog = mapM updateRegisters' prog
  where updateRegisters' (ADD rd ri rj) = do cd <- getNodeColour rd
                                             ci <- getNodeColour ri
                                             cj <- getNodeColour rj
                                             return $ ADD cd ci cj
        updateRegisters' (SUB rd ri rj) = do cd <- getNodeColour rd
                                             ci <- getNodeColour ri
                                             cj <- getNodeColour rj
                                             return $ ADD cd ci cj
        updateRegisters' (MUL rd ri rj) = do cd <- getNodeColour rd
                                             ci <- getNodeColour ri
                                             cj <- getNodeColour rj
                                             return $ MUL cd ci cj
        updateRegisters' (DIV rd ri rj) = do cd <- getNodeColour rd
                                             ci <- getNodeColour ri
                                             cj <- getNodeColour rj
                                             return $ DIV cd ci cj
        updateRegisters' (AND rd ri rj) = do cd <- getNodeColour rd
                                             ci <- getNodeColour ri
                                             cj <- getNodeColour rj
                                             return $ AND cd ci cj
        updateRegisters' (OR  rd ri rj) = do cd <- getNodeColour rd
                                             ci <- getNodeColour ri
                                             cj <- getNodeColour rj
                                             return $ OR cd ci cj
        updateRegisters' (NOT rd ri   ) = do cd <- getNodeColour rd
                                             ci <- getNodeColour ri
                                             return $ NOT cd ci
        updateRegisters' (JMP    ri   ) = do ci <- getNodeColour ri
                                             return $ JMP ci
        updateRegisters' (BEZ    ri  o) = do ci <- getNodeColour ri
                                             return $ BEZ ci o
        updateRegisters' (CEQ rd ri rj) = do cd <- getNodeColour rd
                                             ci <- getNodeColour ri
                                             cj <- getNodeColour rj
                                             return $ CEQ cd ci cj
        updateRegisters' (CGT rd ri rj) = do cd <- getNodeColour rd
                                             ci <- getNodeColour ri
                                             cj <- getNodeColour rj
                                             return $ CGT cd ci cj
        updateRegisters' (LDC rd     o) = do cd <- getNodeColour rd
                                             return $ LDC cd o
        updateRegisters' (LDM rd ri   ) = do cd <- getNodeColour rd
                                             ci <- getNodeColour ri
                                             return $ LDM cd ci
        updateRegisters' (STM    ri rj) = do ci <- getNodeColour ri
                                             cj <- getNodeColour rj
                                             return $ STM ci cj
        updateRegisters' i              =    return i


-- |This function colours the register interference graph in the allocator state.
colourGraph :: Int -> State Allocator ()
colourGraph k = do isGEmpty <- isEmpty
                   if isGEmpty
                     then return ()
                     else do n <- do n <- getNodeWithMaxDegree k
                                     if n == Nothing
                                       then getRandomNode
                                       else return $ fromJust n
                             edges <- removeNode n
                             colourGraph k
                             addNode n
                             addEdges edges
                             edgesFrom <- getEdgesFrom n
                             colour <- getColour n edgesFrom
                             setColour n colour

-- |This function returns the maximum colour in the graph.
getMaxColour :: State Allocator Colour
getMaxColour = do nodes <- getNodes
                  let (Node _ c) = maximum nodes
                  return c

-- |This function returns the lowest colour possible for the specified node.
--  The edges should be from the specified node. Also, you can't rely on the
--  colour of the node in the edge, you must get it from the graph as the node
--  in the edge may not have been updated.
getColour :: Node -> [ Edge ] -> State Allocator Colour
getColour (Node n _) es
  = if n == pc then return pc
    else if n == sp then return sp
    else do colours <- mapM (\(Edge _ (Node r _)) -> getNodeColour r) es
            let loop k = if k `elem` colours
                           then loop (k + 1)
                           else return k
            loop (sp + 1)

-- |This function builds the register interference graph for the specified
--  program.
buildInterferenceGraph :: [ Instruction ] -> State Allocator ()
buildInterferenceGraph prog
  = mapM_ (\n -> do
      def <- getDef n
      case def of
        [ d ] -> do addNode $ Node d 0
                    out_n <- getOut n
                    let edgeNodes = out_n \\ [ d ]
                    mapM_ (\n -> addEdge $ Edge (Node d 0) (Node n 0)) edgeNodes
        _      -> return ()) prog

-- |This function builds the in and out sets for the specified program.
analyseLiveRegisters :: [ Instruction ] -> State Allocator ()
analyseLiveRegisters prog
  = do truth_l <- mapM (\n -> do in_n' <- getIn n
                                 out_n' <- getOut n
                                 sucs_n <- getSuccessors n
                                 out_n <- foldM (\o i -> do in_i <- getIn i
                                                            return $ o `union` in_i) [] sucs_n
                                 use_n <- getUse n
                                 def_n <- getDef n
                                 let in_n = use_n `union` (out_n \\ def_n)
                                 setIn n in_n
                                 setOut n out_n
                                 return (in_n', in_n, out_n', out_n)) prog
       let truth = foldr (\(in_n', in_n, out_n', out_n) t -> t &&
                                                             (in_n' == in_n) &&
                                                             (out_n' == out_n)) True truth_l
       if truth
         then return ()
         else analyseLiveRegisters prog

-- |This function builds the use and def sets for the specified program.
buildUseDef :: [ Instruction ] -> State Allocator ()
buildUseDef prog = mapM_ buildUseDef' prog
  where buildUseDef' i@(ADD rd ri rj) = do setDef i rd
                                           setUse i $ nub [ pc, sp, ri, rj ]
        buildUseDef' i@(SUB rd ri rj) = do setDef i rd
                                           setUse i $ nub [ pc, sp, ri, rj ]
        buildUseDef' i@(MUL rd ri rj) = do setDef i rd
                                           setUse i $ nub [ pc, sp, ri, rj ]
        buildUseDef' i@(DIV rd ri rj) = do setDef i rd
                                           setUse i $ nub [ pc, sp, ri, rj ]
        buildUseDef' i@(AND rd ri rj) = do setDef i rd
                                           setUse i $ nub [ pc, sp, ri, rj ]
        buildUseDef' i@(OR  rd ri rj) = do setDef i rd
                                           setUse i $ nub [ pc, sp, ri, rj ]
        buildUseDef' i@(NOT rd ri   ) = do setDef i rd
                                           setUse i $ nub [ pc, sp, ri ]
        buildUseDef' i@(JMP    ri   ) =    setUse i $ nub [ pc, sp, ri ]
        buildUseDef' i@(BEZ    ri  o) =    setUse i $ nub [ pc, sp, ri ]
        buildUseDef' i@(CEQ rd ri rj) = do setDef i rd
                                           setUse i $ nub [ pc, sp, ri, rj ]
        buildUseDef' i@(CGT rd ri rj) = do setDef i rd
                                           setUse i $ nub [ pc, sp, ri, rj ]
        buildUseDef' i@(LDC rd     o) = do setDef i rd
                                           setUse i $ nub [ pc, sp ]
        buildUseDef' i@(LDM rd ri   ) = do setDef i rd
                                           setUse i $ nub [ pc, sp, ri ]
        buildUseDef' i@(STM    ri rj) =    setUse i $ nub [ pc, sp, ri, rj ]
        buildUseDef' i                =    setUse i $ nub [ pc, sp ]

-- |This function builds the map of labels to their following instructions.
buildLabelMap :: [ Instruction ] -> State Allocator ()
buildLabelMap prog = mapM_ buildLabelMap' $ zip prog (drop 1 prog)
  where buildLabelMap' (LABEL l, next_i)
          = setLabelInst l next_i
        buildLabelMap' _
          = return ()

-- |This function builds the successor set for each instruction in the specified
--  program.
buildSuccessorSet :: [ Instruction ] -> State Allocator ()
buildSuccessorSet prog = mapM_ buildSuccessorSet' $ zip prog (drop 1 prog)
  where buildSuccessorSet' (LABEL l, next_i)
          = return ()
        buildSuccessorSet' (i@(BEZ ri (Right l)), next_i)
          = do inst <- getLabelInst l
               addSuccessor i inst
               addSuccessor i next_i
        buildSuccessorSet' (i@(LDC rd (Right l)), next_i@(JMP ri))
          = do inst <- getLabelInst l
               addSuccessor i next_i
               addSuccessor next_i inst
        buildSuccessorSet' (i@(JMP ri), next_i)
          = do sucs <- getSuccessors i
               if length sucs == 0
                 then return ()
                 else addSuccessor i next_i
        buildSuccessorSet' (i, next_i)
          = addSuccessor i next_i

-- |This function returns the instruction mapped to the specified label.
getLabelInst :: Label -> State Allocator Instruction
getLabelInst l = do s <- get
                    return . fromJust . Map.lookup l $ (labels s)

-- |This function sets the instruction to be mapped to the specified label.
setLabelInst :: Label -> Instruction -> State Allocator ()
setLabelInst l i = do s <- get
                      put $ s { labels = Map.insert l i (labels s) }

-- |This function returns the successor set for the specified instruction.
getSuccessors :: Instruction -> State Allocator [ Instruction ]
getSuccessors i = do s <- get
                     return . Map.findWithDefault [] i $ (successors s)

-- |This function adds and instruction to the successor set of the specified
--  instruction.
addSuccessor :: Instruction -> Instruction -> State Allocator ()
addSuccessor i suc = do s <- get
                        sucs <- getSuccessors i
                        put $ s { successors = Map.insert i (suc:sucs) (successors s) }

-- |This function returns the use set associated with the specified instruction.
getUse :: Instruction -> State Allocator [ Register ]
getUse i = do s <- get
              return . Map.findWithDefault [] i $ (useSet s)

-- |This function sets the use set to be associated with the specified instruction.
setUse :: Instruction -> [ Register ] -> State Allocator ()
setUse i r = do s <- get
                put $ s { useSet = Map.insert i r (useSet s) }

-- |This function returns the def set associated with the specified instruction.
getDef :: Instruction -> State Allocator [ Register ]
getDef i = do s <- get
              return . Map.findWithDefault [] i $ (defSet s)

-- |This function sets the def set to be associated with the specified instruction.
setDef :: Instruction -> Register -> State Allocator ()
setDef i r = do s <- get
                put $ s { defSet = Map.insert i [ r ] (defSet s) }

-- |This function returns the in set associated with the specidied instruction.
getIn :: Instruction -> State Allocator [ Register ]
getIn i = do s <- get
             return . Map.findWithDefault [] i $ (inSet s)

-- |This function sets the in set to be associated with the specified instruction.
setIn :: Instruction -> [ Register ] -> State Allocator ()
setIn i r = do s <- get
               put $ s { inSet = Map.insert i r (inSet s) }

-- |This function returns the out set associated with the specified instruction.
getOut :: Instruction -> State Allocator [ Register ]
getOut i = do s <- get
              return . Map.findWithDefault [] i $ (outSet s)

-- |This function sets the out set to be associated with the specified instruction.
setOut :: Instruction -> [ Register ] -> State Allocator ()
setOut i r = do s <- get
                put $ s { outSet = Map.insert i r (outSet s) }

-- |This function adds the specified node to the register interference graph.
addNode :: Node -> State Allocator ()
addNode n = do s <- get
               let (Graph ns es) = graph s
               if n `elem` ns
                 then return ()
                 else put $ s { graph = Graph (n:ns) es }

-- |This function returns the node associated with the specified register.
getNode :: Register -> State Allocator Node
getNode r = do ns <- getNodes
               return . head . filter (\(Node reg _) -> reg == r) $ ns

-- |This function returns the colour associated with the specified register.
getNodeColour :: Register -> State Allocator Colour
getNodeColour r = do (Node _ c) <- getNode r
                     return c

-- |This function returns all nodes in the register interference graph.
getNodes :: State Allocator [ Node ]
getNodes = do s <- get
              let (Graph ns es) = graph s
              return ns

-- |This function returns a random node from the graph.
getRandomNode :: State Allocator Node
getRandomNode = do ns <- getNodes
                   return $ head ns

-- |This function returns the node with lowest degree less than @k@. If none
--  exist, it returns Nothing.
getNodeWithMaxDegree :: Int -> State Allocator (Maybe Node)
getNodeWithMaxDegree k = do ns <- getNodes
                            node_edges <- mapM getEdgesFrom $ ns
                            let degrees = map length node_edges
                                n_degs = map fst . filter (\(n, d) -> d < k) . zip ns $ degrees
                            return . listToMaybe $ n_degs

-- |This function removes the specified node from the register interference
--  graph, plus its associated edges.
removeNode :: Node -> State Allocator [ Edge ]
removeNode n = do s <- get
                  edges <- getEdgesFrom n
                  let (Graph ns es) = graph s
                      ns' = delete n ns
                      es' = filter (\(Edge s t) -> s /= n && t /= n) es
                  put $ s { graph = Graph ns' es'}
                  return edges

-- |This function adds the specified edge to the register interference graph.
addEdge :: Edge -> State Allocator ()
addEdge e@(Edge s t) = do st <- get
                          let (Graph ns es) = graph st
                          if e `elem` es
                            then return ()
                            else if s `elem` ns && t `elem` ns
                              then put $ st { graph = Graph ns ((Edge t s):e:es) }
                              else return ()

-- |This function adds the specified edges to the register interference graph.
addEdges :: [ Edge ] -> State Allocator ()
addEdges es = mapM_ addEdge es

-- |This function returns the list of edges in the register interference graph.
getEdges :: State Allocator [ Edge ]
getEdges = do s <- get
              let (Graph ns es) = graph s
              return es

-- |This function returns True if there aren't any nodes in the register
--  interference graph.
isEmpty :: State Allocator Bool
isEmpty = do s <- get
             let (Graph ns es) = graph s
             return $ ns == []

-- |This function returns the number of nodes in the register interference graph.
size :: State Allocator Int
size = do ns <- getNodes
          return $ length ns

-- |This function returns the list of edges from the specified node.
getEdgesFrom :: Node -> State Allocator [ Edge ]
getEdgesFrom n = do es <- getEdges
                    return $ filter (\(Edge s _) -> n == s) es

-- |This function sets the colour of the specified node.
setColour :: Node -> Colour -> State Allocator ()
setColour n@(Node r _) c = do s <- get
                              let n' = Node r c
                                  (Graph ns es) = graph s
                                  ns' = delete n ns
                              put $ s { graph = Graph (n':ns') es }
