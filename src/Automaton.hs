module Automaton (createAutomatonIO, expToGraph, Inst(AssSw , AssPt , TestSw , TestPt , Dup , Par , Prob , Drop, Skip), InstNode, PnkGraph) where

import Data.Graph.Inductive.Graph -- (Context, Node, prettyPrint, insNode, insEdge, empty, nodes, edges, insNodes, Graph (mkGraph), labNodes, labEdges, LNode, LEdge, delEdges, delNodes, insEdges, (&))

-- import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Tree (Gr)

import Control.Monad (when)
import Syntax.Par
import Syntax.Abs
import Syntax.Print

import System.Exit ( exitFailure )

type Verbosity = Int

data Inst = AssSw | AssPt | TestSw | TestPt | Dup | Par | Prob | Drop | Skip deriving Show
type InstNode = (Inst, Double)

type PnkGraph = Gr InstNode Double

-- we want to create a function that takes a program and outputs c++ code
-- the program should be turned into a finite automaton


putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

showTree :: (Show a, Print a) => Int -> a -> IO ()
showTree v tree = do
  putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
  putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

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
      let (leftGraph, leftmax ) = expToGraph' e1 graph nodenr parentnr loopback
      let (rightGraph, rightmax) = expToGraph' e2 leftGraph (leftmax + 1) leftmax loopback
      -- let connection = insEdge (leftmax, leftmax + 1, ()) rightGraph
      (rightGraph, rightmax)
    EProb e1 d e2 -> do
      let newGraph = insNode (nodenr, (Prob, d)) graph
      let (leftGraph, leftmax) = expToGraph' e1 newGraph (nodenr + 1) nodenr loopback
      let (rightGraph, rightmax) = expToGraph' e2 leftGraph (leftmax + 1) nodenr loopback
      -- I feel like leftEdge and rightEdge should be handled by the children
      -- but deleting them does not work..
      let leftEdge = insEdge (nodenr, nodenr + 1, d) rightGraph
      let rightEdge = insEdge (nodenr, leftmax + 1, 1-d) leftEdge
      let parentEdge = insEdge (parentnr, nodenr, 1.0) rightEdge
      (parentEdge, rightmax)
    EProbD e1 e2 -> do
      let newGraph = insNode (nodenr, (Prob, 0.5)) graph
      let (leftGraph, leftmax) = expToGraph' e1 newGraph (nodenr + 1) nodenr loopback
      let (rightGraph, rightmax) = expToGraph' e2 leftGraph (leftmax + 1) nodenr loopback
      -- I feel like leftEdge and rightEdge should be handled by the children
      -- but deleting them does not work..
      let leftEdge = insEdge (nodenr, nodenr + 1, 0.5) rightGraph
      let rightEdge = insEdge (nodenr, leftmax + 1, 0.5) leftEdge
      let parentEdge = insEdge (parentnr, nodenr, 1.0) rightEdge
      (parentEdge, rightmax)
    EPar e1 e2 -> do
      let newGraph = insNode (nodenr, (Par, 0)) graph
      let (leftGraph, leftmax) = expToGraph' e1 newGraph (nodenr + 1) nodenr loopback
      let (rightGraph, rightmax) = expToGraph' e2 leftGraph (leftmax + 1) nodenr loopback
      -- I feel like leftEdge and rightEdge should be handled by the children
      -- but deleting them does not work..
      let leftEdge = insEdge (nodenr, nodenr + 1, 1.0) rightGraph
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
  delEdge (0, 0) graph -- remove self loop

createAutomatonIO :: [String] -> IO ()
createAutomatonIO content = do
  putStrLn "createAutomaton"
  s <- readFile (head content)
  let ts = myLexer s
  case pExp ts of
    Left err -> do
      putStrLn "\nParse Failed...\n"
      putStrLn err
      exitFailure
    Right tree -> do
      putStrLn "\nParse Successful!\n"
      showTree 2 tree
      -- traverse the tree and print the c++ code
      putStrLn "C++ code:"
      putStrLn "PnkPrgrm getAutomaton() {"
      putStrLn "\tPnkPrgrm ret;"
      let prgrm = graphToInstructionList $ expToGraph tree
      putStr prgrm
      putStrLn "\n\treturn ret;"
      putStrLn "}"
      putStrLn "writing to file..."
      writeCppFile prgrm


newInstrToString :: LNode InstNode -> String
newInstrToString instrnode = do
  let (_, node) = instrnode
  case node of
    (AssSw, arg) -> "\tret.addNode(SW, " ++ show arg ++ ", 0.0);"
    (AssPt, arg) -> "\tret.addNode(PT, " ++ show arg ++ ", 0.0);"
    (TestSw, arg) -> "\tret.addNode(TESTSW, " ++ show arg ++ ", 0.0);"
    (TestPt, arg) -> "\tret.addNode(TESTPT, " ++ show arg ++ ", 0.0);"
    (Dup, _) -> "\tret.addNode(DUP, 0, 0.0);"
    (Par, _) -> "\tret.addNode(PAR, 0, 0.0);"
    (Prob, arg) -> "\tret.addNode(PROB, 0, " ++ show arg ++ ");"
    (Drop, _) -> "\tret.addNode(DROP, 0, 0.0);"
    (Skip, _) -> "\tret.addNode(SKIP, 0, 0.0);"

newEdgestoString :: LEdge Double -> String
newEdgestoString edge = do
  let (from, to, d) = edge
  "\tret.addEdge(" ++ show from ++ ", " ++ show to ++ ", " ++ show d ++ ");"

graphToInstructionList :: PnkGraph -> String
graphToInstructionList graph = do
  let nodeslist = labNodes graph
  let edgeslist = labEdges graph
  let nodesstring = map newInstrToString nodeslist
  let edgesstring = map newEdgestoString edgeslist
  unlines nodesstring ++ unlines edgesstring

writeCppFile :: String -> IO ()
writeCppFile content = do
  writeFile filename "#ifndef COMPILED_PNK_PROGRAM_H\n#define COMPILED_PNK_PROGRAM_H\n#include \"pnk-program.h\"\n"
  appendFile filename "PnkPrgrm getAutomaton() {\n\tPnkPrgrm ret;\n"
  appendFile filename content
  appendFile filename "\n\treturn ret;\n}"
  appendFile filename "\n#endif"
  where filename = "ns3/compiled-pnk-program.h"