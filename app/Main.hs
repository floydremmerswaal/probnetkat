module Main (Main.main) where
    -- main function

import Prelude hiding (drop, seq)

import System.Environment ( getArgs )
import System.Exit ( exitFailure )
import Control.Monad (when)
import qualified Data.Set as Set
import Syntax.Lex
import Syntax.Abs
import Syntax.Par
import Syntax.Print
import Semantics
import Transformation

import Text.Printf
import Control.Arrow

import Control.Monad.Bayes.Enumerator

import Syntax.ErrM
import Text.Read.Lex (Number, Lexeme (String))

import Data.Tree
import Data.Tree.Pretty


import Data.Graph.Inductive.Graph (prettyPrint, insNode, insEdge, empty, nodes, edges, insNodes, Graph (mkGraph), labNodes, labEdges, LNode, LEdge)
import Data.Graph.Inductive.Basic (gfold)
import Data.Graph.Inductive.PatriciaTree (Gr)

import Data.Graph.Inductive.Dot (fglToDot, showDot)
import Data.Graph.Inductive.Query (bfs, bfe)

type ParseFun a = [Token] -> Err a

type Verbosity = Int

type PnkGraph = Gr InstNode ()

toNormalForm :: PnkGraph -> PnkGraph
toNormalForm graph = do
  -- in normal form, we push the PROB nodes to the root, and we push the SEQ to the leaves
  -- PAR is just before SEQ leaves
  graph

getGraphNodes :: PnkGraph -> IO ()
getGraphNodes graph = do 
  let nodeslist = labNodes graph
  let edgeslist = labEdges graph
  print nodeslist
  print edgeslist

writeGraphToFile :: String -> PnkGraph -> IO ()
writeGraphToFile name graph = do
  let dot = showDot $ fglToDot graph
  writeFile name dot

-- type of Graph is Gr a b with a the type for the nodes and b the type for the edges
testGraph :: Exp -> IO ()
testGraph expression = do
  putStr "testGraph\n"
  let graph = expToGraph expression
  prettyPrint graph
  writeGraphToFile "automaton.dot" graph
  getGraphNodes graph
  putStr $ graphToInstructionList graph

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

newEdgestoString :: LEdge () -> String
newEdgestoString edge = do
  let (from, to, _) = edge
  "\tret.addEdge(" ++ show from ++ ", " ++ show to ++ ");"

graphToInstructionList :: PnkGraph -> String
graphToInstructionList graph = do
  let nodeslist = labNodes graph
  let edgeslist = labEdges graph
  let nodesstring = map newInstrToString nodeslist
  let edgesstring = map newEdgestoString edgeslist
  unlines nodesstring ++ unlines edgesstring

expToGraph' :: Exp -> PnkGraph -> Int -> Int -> (PnkGraph, Int)
expToGraph' expression graph nodenr parentnr =
  case expression of
    EAssPt arg -> do
      let newGraph = insNode (nodenr, (AssPt, fromInteger arg)) graph
      let addedEdge = insEdge (parentnr, nodenr, ()) newGraph
      (addedEdge, nodenr)
    EAssSw arg -> do
      let newGraph = insNode (nodenr, (AssSw, fromInteger arg)) graph
      let addedEdge = insEdge (parentnr, nodenr, ()) newGraph
      (addedEdge, nodenr)
    ESwEq arg -> do
      let newGraph = insNode (nodenr, (TestSw, fromInteger arg)) graph
      let addedEdge = insEdge (parentnr, nodenr, ()) newGraph
      (addedEdge, nodenr)
    EPtEq arg -> do
      let newGraph = insNode (nodenr, (TestPt, fromInteger arg)) graph
      let addedEdge = insEdge (parentnr, nodenr, ()) newGraph
      (addedEdge, nodenr)
    ESwNEq arg -> do
      let newGraph = insNode (nodenr, (TestSw, fromInteger arg)) graph
      let addedEdge = insEdge (parentnr, nodenr, ()) newGraph
      (addedEdge, nodenr)
    EPtNEq arg -> do
      let newGraph = insNode (nodenr, (TestPt, fromInteger arg)) graph
      let addedEdge = insEdge (parentnr, nodenr, ()) newGraph
      (addedEdge, nodenr)
    EDup -> do
      let newGraph = insNode (nodenr, (Dup, 0)) graph
      let addedEdge = insEdge (parentnr, nodenr, ()) newGraph
      (addedEdge, nodenr)
    ESkip -> do
      let newGraph = insNode (nodenr, (Skip, 0)) graph
      let addedEdge = insEdge (parentnr, nodenr, ()) newGraph
      (addedEdge, nodenr)
    EDrop -> do
      let newGraph = insNode (nodenr, (Drop, 0)) graph
      let addedEdge = insEdge (parentnr, nodenr, ()) newGraph
      (addedEdge, nodenr)
    ESeq e1 e2 -> do
      let (leftGraph, leftmax ) = expToGraph' e1 graph nodenr parentnr 
      let (rightGraph, rightmax) = expToGraph' e2 leftGraph (leftmax + 1) leftmax
      -- let connection = insEdge (leftmax, leftmax + 1, ()) rightGraph
      (rightGraph, rightmax)
    EProb e1 d e2 -> do
      let (leftGraph, leftmax) = expToGraph' e1 graph (nodenr + 1) nodenr
      let (rightGraph, rightmax) = expToGraph' e2 leftGraph (leftmax + 1) nodenr
      let newGraph = insNode (nodenr, (Prob, d)) rightGraph
      -- I feel like leftEdge and rightEdge should be handled by the children
      -- but deleting them does not work..
      let leftEdge = insEdge (nodenr, nodenr + 1, ()) newGraph
      let rightEdge = insEdge (nodenr, leftmax + 1, ()) leftEdge
      let parentEdge = insEdge (parentnr, nodenr, ()) rightEdge
      (parentEdge, rightmax)
    EProbD e1 e2 -> do
      let (leftGraph, leftmax) = expToGraph' e1 graph (nodenr + 1) nodenr
      let (rightGraph, rightmax) = expToGraph' e2 leftGraph (leftmax + 1) nodenr
      let newGraph = insNode (nodenr, (Prob, 0.5)) rightGraph
      -- I feel like leftEdge and rightEdge should be handled by the children
      -- but deleting them does not work..
      let leftEdge = insEdge (nodenr, nodenr + 1, ()) newGraph
      let rightEdge = insEdge (nodenr, leftmax + 1, ()) leftEdge
      let parentEdge = insEdge (parentnr, nodenr, ()) rightEdge
      (parentEdge, rightmax)
    EPar e1 e2 -> do
      let (leftGraph, leftmax) = expToGraph' e1 graph (nodenr + 1) nodenr
      let (rightGraph, rightmax) = expToGraph' e2 leftGraph (leftmax + 1) nodenr
      let newGraph = insNode (nodenr, (Par, 0)) rightGraph
      -- I feel like leftEdge and rightEdge should be handled by the children
      -- but deleting them does not work..
      let leftEdge = insEdge (nodenr, nodenr + 1, ()) newGraph
      let rightEdge = insEdge (nodenr, leftmax + 1, ()) leftEdge
      let parentEdge = insEdge (parentnr, nodenr, ()) rightEdge
      (parentEdge, rightmax)
    EKleene e1 -> do 
      -- in the graph version, we no longer need kleene nodes
      -- we will just loop back to the beginning
      let (childGraph, childmax) = expToGraph' e1 graph nodenr parentnr
      let newGraph = insEdge (childmax, nodenr, ()) childGraph
      (newGraph, childmax)



expToGraph :: Exp -> PnkGraph
expToGraph expression = do 
   -- insert a skip in the beginning as no-op
   -- to prevent self loop at the beginning
  let thegraph = empty  :: PnkGraph
  let seeded = insNode (0, (Skip, 0)) thegraph
  let (graph, _) = expToGraph' expression seeded 1 0
  graph

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

runFile :: (Print a, Show a) => Verbosity -> ParseFun a -> FilePath -> IO ()
runFile v p f = putStrLn f >> readFile f >>= run v p

run :: (Print a, Show a) => Verbosity -> ParseFun a -> String -> IO ()
run v p s =
  case p ts of
    Left err -> do
      putStrLn "\nParse              Failed...\n"
      putStrV v "Tokens:"
      mapM_ (putStrV v . showPosToken . mkPosToken) ts
      putStrLn err
      exitFailure
    Right tree -> do
      putStrLn "\nParse Successful!"
      showTree v tree
  where
  ts = myLexer s
  showPosToken ((l,c),t) = concat [ show l, ":", show c, "\t", show t ]

showTree :: (Show a, Print a) => Int -> a -> IO ()
showTree v tree = do
  putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
  putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  -c               Compile to C++"
    , "  --help          Display this help message."
    , "  (no arguments)  Parse stdin verbosely."
    , "  --test (files)      Run test on content of files."
    , "  (files)         Parse content of files verbosely."
    , "  -s (files)      Silent mode. Parse content of files silently."
    , "  --auto (files)    Create automaton from file."
    ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    --[]         -> getContents >>= run 2 pExp
    []        -> testGraph (ESeq (EAssSw 1) (EAssPt 1))
    "-s":fs    -> mapM_ (runFile 0 pExp) fs
    "--test":fs  -> testF fs
    "--auto":fs -> createAutomaton fs
    "-c":fs    -> createAutomaton fs
    fs         -> mapM_ (runFile 2 pExp) fs


prettyPrintSHD :: [(SH, Double)] -> IO ()
prettyPrintSHD [] = return ()
prettyPrintSHD ((sh, d):xs) = do
  putStrLn $ printf "%.2f" (d * 100) ++ "%" ++ " : " ++ show sh
  prettyPrintSHD xs

testF :: [String] ->  IO ()
testF fs = do
  putStrLn "test"
  s <- readFile (head fs)
  let ts = myLexer s
  case pExp ts of
    Left err -> do
      putStrLn "\nParse Failed...\n"
      putStrLn err
      exitFailure
    Right tree -> do
      putStrLn "\nParse Successful!"
      print tree
      let packet = (1,1) :: Packet
      let history = [packet] :: History
      let initialSet = Set.fromList [history] :: SH
      let kleisliArrow = transExp tree ::  Kleisli Enumerator SH SH
      putStrLn "Function is defined"
      let result = runKleisli kleisliArrow initialSet
      let samples = enumerator result
      putStrLn "Function output:"
      prettyPrintSHD samples

data Inst = AssSw | AssPt | TestSw | TestPt | Dup | Par | Prob | Drop | Skip deriving Show
type InstNode = (Inst, Double)

type AutomatonNode = (Inst, Double, Int, Int)


instrToCppString :: Int -> Int -> InstNode -> String
instrToCppString _ parentnr instrnode = do
  case instrnode of
    (AssSw, arg) -> "\tret.addNode(" ++ show parentnr ++ ",  SW, " ++ show arg ++ ", 0.0);"
    (AssPt, arg) -> "\tret.addNode(" ++ show parentnr ++ ",  PT, " ++ show arg ++ ", 0.0);"
    (TestSw, arg) -> "\tret.addNode(" ++ show parentnr ++ ",  TESTSW, " ++ show arg ++ ", 0.0);"
    (TestPt, arg) -> "\tret.addNode(" ++ show parentnr ++ ",  TESTPT, " ++ show arg ++ ", 0.0);"
    (Dup, _) -> "\tret.addNode(" ++ show parentnr ++ ",  DUP, 0, 0.0);"
    (Par, _) -> "\tret.addNode(" ++ show parentnr ++ ",  PAR, 0, 0.0);"
    (Prob, arg) -> "\tret.addNode(" ++ show parentnr ++ ",  PROB, 0, " ++ show arg ++ ");"
    (Drop, _) -> "\tret.addNode(" ++ show parentnr ++ ",  DROP, 0, 0.0);"
    (Skip, _) -> "\tret.addNode(" ++ show parentnr ++ ",  SKIP, 0, 0.0);"

printAutomaton :: Tree InstNode -> IO ()
printAutomaton = putStrLn . drawVerticalTree . transferInstTreeToStringTree

transferInstTreeToStringTree :: Tree InstNode -> Tree String
transferInstTreeToStringTree (Node (x, y) []) = Node (show x ++ " " ++ show y) []
transferInstTreeToStringTree (Node (x, y) [leftTree]) = do
  let newLeftTree = transferInstTreeToStringTree leftTree
  Node (show x ++ " " ++ show y) [newLeftTree]
transferInstTreeToStringTree (Node (x, y) (leftTree:rightTree)) = do
  let newLeftTree = transferInstTreeToStringTree leftTree
  let newRightTree = transferInstTreeToStringTree (head rightTree)
  Node (show x ++ " " ++ show y) [newLeftTree, newRightTree]

-- we want to create a function that takes a program and outputs c++ code
-- the program should be turned into a finite automaton
createAutomaton :: [String] -> IO ()
createAutomaton content = do
  putStrLn "createAutomaton"
  s <- readFile (head content)
  let ts = myLexer s
  case pExp ts of
    Left err -> do
      putStrLn "\nParse Failed...\n"
      putStrLn err
      exitFailure
    Right tree -> do
      -- temporarily hijacked for testing purposes
      putStrLn "\nParse Successful!\n"
      testGraph tree
      -- putStrLn "\nParse Successful!\n"
      -- showTree 2 tree
      -- -- traverse the tree and print the c++ code
      -- putStrLn "Tree:"
      -- printAutomaton $ expToTree tree
      -- putStrLn "C++ code:"
      -- putStrLn "PnkPrgrm getAutomaton() {"
      -- putStrLn "\tPnkPrgrm ret;"
      -- let prgrm = getStringFromExp tree
      -- putStr prgrm
      -- putStrLn "\n\treturn ret;"
      -- putStrLn "}"
      -- putStrLn "writing to file..."
      -- writeCppFile prgrm




appendToLeaves :: Tree InstNode -> Tree InstNode -> Tree InstNode
-- if the subtree is empty, we have reached a leaf and need to dump the subtree there
appendToLeaves (Node (x,y) []) someSubTree = Node (x,y) [someSubTree]
appendToLeaves (Node (x,y) [leftTree]) someSubTree = do
  let newLeftTree = appendToLeaves leftTree someSubTree
  Node (x,y) [newLeftTree]
appendToLeaves (Node (x,y) (leftTree:rightTree)) someSubTree = do
  let newLeftTree = appendToLeaves leftTree someSubTree
  let newRightTree = appendToLeaves (head rightTree)someSubTree
  Node (x,y) [newLeftTree, newRightTree]  


writeCppFile :: String -> IO ()
writeCppFile content = do
  writeFile filename "#ifndef COMPILED_PNK_PROGRAM_H\n#define COMPILED_PNK_PROGRAM_H\n#include \"pnk-program.h\"\n"
  appendFile filename "PnkPrgrm getAutomaton() {\n\tPnkPrgrm ret;\n"
  appendFile filename content
  appendFile filename "\n\treturn ret;\n}"
  appendFile filename "\n#endif"
  where filename = "ns3/compiled-pnk-program.h"
-- idea, do a second pass through the tree and construct another tree containing all the information (node number, parent number, instruction)

getAutomatonTree' :: Int -> Int -> Tree InstNode -> (Tree AutomatonNode, Int)
getAutomatonTree' nodenr parentnr (Node (x,y) []) = (Node (x,y, nodenr, parentnr) [], nodenr + 1)
getAutomatonTree' nodenr parentnr (Node (x,y) [leftTree]) = do
  let newLeftTree = getAutomatonTree' (nodenr + 1) nodenr leftTree
  (Node (x,y, nodenr, parentnr) [fst newLeftTree], snd newLeftTree)
getAutomatonTree' nodenr parentnr (Node (x,y) (leftTree:rightTree)) = do
  let newLeftTree = getAutomatonTree' (nodenr + 1) nodenr leftTree
  let newRightTree = getAutomatonTree' (snd newLeftTree) nodenr (head rightTree)
  (Node (x,y, nodenr, parentnr) [fst newLeftTree, fst newRightTree], snd newRightTree)


getAutomatonTree :: Tree InstNode -> Tree AutomatonNode
getAutomatonTree (Node (x,y) z) = fst $ getAutomatonTree' 0 0 (Node (x,y) z)

automatonToCppString :: Tree AutomatonNode -> String
automatonToCppString (Node (x,y,z,w) []) = instrToCppString z w (x,y)
automatonToCppString (Node (x,y,z,w) [leftTree]) = do
  let newLeftTree = automatonToCppString leftTree
  instrToCppString z w (x,y) ++ "\n" ++ newLeftTree
automatonToCppString (Node (x,y,z,w) (leftTree:rightTree)) = do
  let newLeftTree = automatonToCppString leftTree
  let newRightTree = automatonToCppString (head rightTree)
  instrToCppString z w (x,y) ++ "\n" ++ newLeftTree ++ "\n" ++ newRightTree