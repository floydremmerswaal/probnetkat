module Automaton (compileGraph, createAutomaton, createAutomatonIO, expToGraph, Inst(AssSw , AssPt , TestSw , TestPt , Dup , Par , Prob , Drop, Skip), InstNode, PnkGraph) where

import Data.Graph.Inductive.Graph -- (Context, Node, prettyPrint, insNode, insEdge, empty, nodes, edges, insNodes, Graph (mkGraph), labNodes, labEdges, LNode, LEdge, delEdges, delNodes, insEdges, (&))

import Data.Graph.Inductive.PatriciaTree (Gr)
-- import Data.Graph.Inductive.Tree (Gr)
import Syntax.Abs

data Inst = AssSw | AssPt | TestSw | TestPt | Dup | Par | Prob | Drop | Skip deriving Show
type InstNode = (Inst, Double)

type PnkGraph = Gr InstNode Double

expToGraph' :: Exp -> PnkGraph -> Int -> Int -> Int -> (PnkGraph, Int)
expToGraph' expression graph nodenr parentnr loopback =
  case expression of
    EAssPt arg -> do
      let newGraph = insNode (nodenr, (AssPt, fromInteger arg)) graph
      let addedEdge = insEdge (parentnr, nodenr, 1.0) newGraph
      if loopback >= 0 then 
          (insEdge (nodenr, loopback, 1.0) addedEdge, nodenr)
        else
          (addedEdge, nodenr)
    EAssSw arg -> do
      let newGraph = insNode (nodenr, (AssSw, fromInteger arg)) graph
      let addedEdge = insEdge (parentnr, nodenr, 1.0) newGraph
      if loopback >= 0 then 
          (insEdge (nodenr, loopback, 1.0) addedEdge, nodenr)
        else
          (addedEdge, nodenr)
    ESwEq arg -> do
      let newGraph = insNode (nodenr, (TestSw, fromInteger arg)) graph
      let addedEdge = insEdge (parentnr, nodenr, 1.0) newGraph
      if loopback >= 0 then 
          (insEdge (nodenr, loopback, 1.0) addedEdge, nodenr)
        else
          (addedEdge, nodenr)
    EPtEq arg -> do
      let newGraph = insNode (nodenr, (TestPt, fromInteger arg)) graph
      let addedEdge = insEdge (parentnr, nodenr, 1.0) newGraph
      if loopback >= 0 then 
          (insEdge (nodenr, loopback, 1.0) addedEdge, nodenr)
        else
          (addedEdge, nodenr)
    EDup -> do
      let newGraph = insNode (nodenr, (Dup, 0)) graph
      let addedEdge = insEdge (parentnr, nodenr, 1.0) newGraph
      if loopback >= 0 then 
          (insEdge (nodenr, loopback, 1.0) addedEdge, nodenr)
        else
          (addedEdge, nodenr)
    ESkip -> do
      let newGraph = insNode (nodenr, (Skip, 0)) graph
      let addedEdge = insEdge (parentnr, nodenr, 1.0) newGraph
      if loopback >= 0 then 
          (insEdge (nodenr, loopback, 1.0) addedEdge, nodenr)
        else
          (addedEdge, nodenr)
    EDrop -> do
      let newGraph = insNode (nodenr, (Drop, 0)) graph
      let addedEdge = insEdge (parentnr, nodenr, 1.0) newGraph
      if loopback >= 0 then 
          (insEdge (nodenr, loopback, 1.0) addedEdge, nodenr)
        else
          (addedEdge, nodenr)
    ESeq e1 e2 -> do
      let (leftGraph, leftmax ) = expToGraph' e1 graph nodenr parentnr (-1)
      let (rightGraph, rightmax) = expToGraph' e2 leftGraph (leftmax + 1) leftmax loopback
      -- let connection = insEdge (leftmax, leftmax + 1, ()) rightGraph
      (rightGraph, rightmax)
    EProb e1 d e2 -> do
      let (leftGraph, leftmax) = expToGraph' e1 graph (nodenr + 1) nodenr loopback
      let (rightGraph, rightmax) = expToGraph' e2 leftGraph (leftmax + 1) nodenr loopback
      let newGraph = insNode (nodenr, (Prob, d)) rightGraph
      -- I feel like leftEdge and rightEdge should be handled by the children
      -- but deleting them does not work..
      let leftEdge = insEdge (nodenr, nodenr + 1, d) newGraph
      let rightEdge = insEdge (nodenr, leftmax + 1, 1-d) leftEdge
      let parentEdge = insEdge (parentnr, nodenr, 1.0) rightEdge
      (parentEdge, rightmax)
    EProbD e1 e2 -> do
      let (leftGraph, leftmax) = expToGraph' e1 graph (nodenr + 1) nodenr loopback
      let (rightGraph, rightmax) = expToGraph' e2 leftGraph (leftmax + 1) nodenr loopback
      let newGraph = insNode (nodenr, (Prob, 0.5)) rightGraph
      -- I feel like leftEdge and rightEdge should be handled by the children
      -- but deleting them does not work..
      let leftEdge = insEdge (nodenr, nodenr + 1, 0.5) newGraph
      let rightEdge = insEdge (nodenr, leftmax + 1, 0.5) leftEdge
      let parentEdge = insEdge (parentnr, nodenr, 1.0) rightEdge
      (parentEdge, rightmax)
    EPar e1 e2 -> do
      let (leftGraph, leftmax) = expToGraph' e1 graph (nodenr + 1) nodenr loopback
      let (rightGraph, rightmax) = expToGraph' e2 leftGraph (leftmax + 1) nodenr loopback
      let newGraph = insNode (nodenr, (Par, 0)) rightGraph
      -- I feel like leftEdge and rightEdge should be handled by the children
      -- but deleting them does not work..
      let leftEdge = insEdge (nodenr, nodenr + 1, 1.0) newGraph
      let rightEdge = insEdge (nodenr, leftmax + 1, 1.0) leftEdge
      let parentEdge = insEdge (parentnr, nodenr, 1.0) rightEdge
      (parentEdge, rightmax)
    EKleene e1 -> do
      -- in the graph version, we no longer need kleene nodes
      -- we will just loop back to the beginning
      expToGraph' e1 graph nodenr parentnr nodenr


expToGraph :: Exp -> PnkGraph
expToGraph expression = do
  let thegraph = empty  :: PnkGraph
  let graph = fst $ expToGraph' expression thegraph 0 0 (-1)
  let graph' = delEdge (0, 0) graph -- remove self loop
  mkGraph (labNodes graph') (labEdges graph') -- 

createAutomaton :: Exp -> PnkGraph
createAutomaton = expToGraph

createAutomatonIO :: Exp -> IO ()
createAutomatonIO expr = do
  putStrLn "Creating automaton..."
  compileGraph $ createAutomaton expr

compileGraph :: PnkGraph -> IO ()
compileGraph graph = do
  putStrLn "Writing to file..."
  writeCppFile $ graphToInstructionList graph

instrToSTring :: LNode InstNode -> String
instrToSTring instrnode = do
  let (_, node) = instrnode
  case node of
    (AssSw, arg) -> "\tret.addNode(SW, " ++ show arg ++ ");"
    (AssPt, arg) -> "\tret.addNode(PT, " ++ show arg ++ ");"
    (TestSw, arg) -> "\tret.addNode(TESTSW, " ++ show arg ++ ");"
    (TestPt, arg) -> "\tret.addNode(TESTPT, " ++ show arg ++ ");"
    (Dup, _) -> "\tret.addNode(DUP, 0);"
    (Par, _) -> "\tret.addNode(PAR, 0);"
    (Prob, _) -> "\tret.addNode(PROB, 0);"
    (Drop, _) -> "\tret.addNode(DROP, 0);"
    (Skip, _) -> "\tret.addNode(SKIP, 0);"

edgesToString :: LEdge Double -> String
edgesToString edge = do
  let (from, to, d) = edge
  "\tret.addEdge(" ++ show from ++ ", " ++ show to ++ ", " ++ show d ++ ");"

graphToInstructionList :: PnkGraph -> String
graphToInstructionList graph = do
  let nodeslist = labNodes graph
  let edgeslist = labEdges graph
  let nodesstring = map instrToSTring nodeslist
  let edgesstring = map edgesToString edgeslist
  unlines nodesstring ++ unlines edgesstring

writeCppFile :: String -> IO ()
writeCppFile content = do
  writeFile filename "#ifndef COMPILED_PNK_PROGRAM_H\n#define COMPILED_PNK_PROGRAM_H\n#include \"pnk-program.h\"\n"
  appendFile filename "PnkPrgrm getAutomaton() {\n\tPnkPrgrm ret;\n"
  appendFile filename content
  appendFile filename "\n\treturn ret;\n}"
  appendFile filename "\n#endif"
  where filename = "ns3/compiled-pnk-program.h"