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

import Data.Tree
import Data.Tree.Pretty


import Data.Graph.Inductive.Graph (Context, Node, prettyPrint, insNode, insEdge, empty, nodes, edges, insNodes, Graph (mkGraph), labNodes, labEdges, LNode, LEdge, delEdges, delNodes, insEdges, (&))
import Data.Graph.Inductive.Basic (gfold)
import Data.Graph.Inductive.PatriciaTree (Gr)

import Data.Graph.Inductive.Dot (fglToDot, showDot)

import Debug.Trace

type ParseFun a = [Token] -> Err a

type Verbosity = Int

data Inst = AssSw | AssPt | TestSw | TestPt | Dup | Par | Prob | Drop | Skip deriving Show
type InstNode = (Inst, Double)

type AutomatonNode = (Inst, Double, Int, Int)

type PnkGraph = Gr InstNode Double
type PnkContext = Context InstNode Double


-- gfold :: Graph gr	 
-- => (Context a b -> [Node])	-- direction of fold
-- -> (Context a b -> c -> d) -- depth aggregation	
-- -> (Maybe d -> c -> c, c)	-- breadth/level aggregation
-- -> [Node]	 
-- -> gr a b	 
-- -> c

-- Node is an Int, Context is a tuple of (pre, node, label, post)
-- pre the incoming, post the outgoing edges

-- => (Context a b -> [Node])	-- direction of fold
di :: (PnkContext -> [Node])
di context = do
  let (_, node, _, post) = context
  let nextnodes = map snd post
  trace ("Visiting node: " ++ show node ++ " with next nodes: " ++ show nextnodes ++ " context: " ++  show context) nextnodes

-- da :: (Context a b -> c -> d) -- depth aggregation	
-- The function takes a context, and combines it with the accumulated value (c),
-- returning a possibly different type (d).
da :: PnkContext -> PnkGraph -> PnkGraph -- for now, merge the context into the graph
da context g =
  trace ("Depth aggregation for graph: " ++ show g ++ " context: " ++ show context) (context & g)

-- Maybe d -> c -> c
combineGraphs :: Maybe PnkGraph -> PnkGraph -> PnkGraph -- insert the nodes and edges of the maybeGraph into the graph
combineGraphs maybeGraph graph =
  case maybeGraph of
    Nothing -> graph -- Nothing case shouldn't happen, but we have to account for it
    Just g -> do 
      let ns = labNodes g
      let es = labEdges g
      trace ("Adding graph " ++ show g ++ "\ngraph = " ++ show graph) $ insEdges es $ insNodes ns graph

-- ba :: (Maybe d -> c -> c, c)	-- breadth/level aggregation
ba :: (Maybe PnkGraph -> PnkGraph -> PnkGraph, PnkGraph)
ba = (trace "Breadth aggregation" combineGraphs, empty)

testGfold :: IO ()
testGfold = do
  let g = empty :: PnkGraph
  let g2 = insNode (0, (AssSw, 1.0)) g
  let g3 = insNode (1, (AssPt, 2.0)) g2
  let g4 = insNode (2, (Par, 0.0)) g3
  let g5 = insNode (3, (Prob, 0.5)) g4
  let g6 = insNode (4, (Prob, 0.5)) g5
  let g7 = insNode (5, (Skip, 0.0)) g6
  let g8 = insNode (6, (Skip, 0.0)) g7
  let g9 = insNode (7, (Skip, 0.0)) g8
  let g10 = insNode (8, (Skip, 0.0)) g9

  let g11 = insEdge (0, 1, 1.0) g10
  let g12 = insEdge (1, 2, 1.0) g11
  let g13 = insEdge (2, 3, 1.0) g12
  let g14 = insEdge (2, 4, 1.0) g13
  let g15 = insEdge (3, 5, 0.5) g14
  let g16 = insEdge (3, 6, 0.5) g15
  let g17 = insEdge (4, 7, 0.5) g16
  let g18 = insEdge (4, 8, 0.5) g17  
  -- write to file
  writeGraphToFile "test.dot" g18
  writeGraphToFile "gfold.dot" $ gfold di da ba [0] g18

toNormalForm :: PnkGraph -> PnkGraph
toNormalForm graph = do
  -- in normal form, we push the PROB nodes to the root, and we push the SEQ to the leaves
  -- PAR is just before SEQ leaves
  -- we need to find the PAR nodes
  let newgraph = empty :: PnkGraph
  -- insert prob node, probability should be determined when making the edges
  let withProb = insNode (0, (Prob, 0.0)) newgraph


  newgraph

getGraphNodes :: PnkGraph -> IO ()
getGraphNodes graph = do
  let nodeslist = labNodes graph
  let edgeslist = labEdges graph
  print nodeslist
  print edgeslist

writeGraphToFile :: (Show a, Show b) => String -> Gr a b -> IO ()
writeGraphToFile name graph = do
  let dot = showDot $ fglToDot graph
  writeFile name dot

-- if a PROB node is already found
expNeedsNormalization' :: Exp -> Bool
expNeedsNormalization' expression = do
  case expression of
    EAssPt _ -> False
    EAssSw _ -> False
    ESwEq _ -> False
    EPtEq _ -> False
    EDup -> False
    ESkip -> False
    EDrop -> False
    ESeq e1 e2 -> expNeedsNormalization' e1 || expNeedsNormalization' e2
    EProbD _ _ -> True
    EProb _ _ _ -> True
    EPar e1 e2 -> expNeedsNormalization' e1 || expNeedsNormalization' e2
    EKleene e1 -> expNeedsNormalization' e1

-- final tree needs to be normalized if it contains a PROB node under a PAR node somewhere
expNeedsNormalization :: Exp -> Bool
expNeedsNormalization expression = do
  case expression of
    EAssPt _ -> False
    EAssSw _ -> False
    ESwEq _ -> False
    EPtEq _ -> False
    EDup -> False
    ESkip -> False
    EDrop -> False
    ESeq e1 e2 -> expNeedsNormalization e1 || expNeedsNormalization e2
    EProbD e1 e2 -> expNeedsNormalization' e1 || expNeedsNormalization' e2
    EProb e1 _ e2 -> expNeedsNormalization' e1 || expNeedsNormalization' e2
    EPar e1 e2 -> expNeedsNormalization e1 || expNeedsNormalization e2
    EKleene e1 -> expNeedsNormalization e1



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

expToGraph' :: Exp -> PnkGraph -> Int -> Int -> (PnkGraph, Int)
expToGraph' expression graph nodenr parentnr =
  case expression of
    EAssPt arg -> do
      let newGraph = insNode (nodenr, (AssPt, fromInteger arg)) graph
      let addedEdge = insEdge (parentnr, nodenr, 1.0) newGraph
      (addedEdge, nodenr)
    EAssSw arg -> do
      let newGraph = insNode (nodenr, (AssSw, fromInteger arg)) graph
      let addedEdge = insEdge (parentnr, nodenr, 1.0) newGraph
      (addedEdge, nodenr)
    ESwEq arg -> do
      let newGraph = insNode (nodenr, (TestSw, fromInteger arg)) graph
      let addedEdge = insEdge (parentnr, nodenr, 1.0) newGraph
      (addedEdge, nodenr)
    EPtEq arg -> do
      let newGraph = insNode (nodenr, (TestPt, fromInteger arg)) graph
      let addedEdge = insEdge (parentnr, nodenr, 1.0) newGraph
      (addedEdge, nodenr)
    EDup -> do
      let newGraph = insNode (nodenr, (Dup, 0)) graph
      let addedEdge = insEdge (parentnr, nodenr, 1.0) newGraph
      (addedEdge, nodenr)
    ESkip -> do
      let newGraph = insNode (nodenr, (Skip, 0)) graph
      let addedEdge = insEdge (parentnr, nodenr, 1.0) newGraph
      (addedEdge, nodenr)
    EDrop -> do
      let newGraph = insNode (nodenr, (Drop, 0)) graph
      let addedEdge = insEdge (parentnr, nodenr, 1.0) newGraph
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
      let leftEdge = insEdge (nodenr, nodenr + 1, d) newGraph
      let rightEdge = insEdge (nodenr, leftmax + 1, 1-d) leftEdge
      let parentEdge = insEdge (parentnr, nodenr, 1.0) rightEdge
      (parentEdge, rightmax)
    EProbD e1 e2 -> do
      let (leftGraph, leftmax) = expToGraph' e1 graph (nodenr + 1) nodenr
      let (rightGraph, rightmax) = expToGraph' e2 leftGraph (leftmax + 1) nodenr
      let newGraph = insNode (nodenr, (Prob, 0.5)) rightGraph
      -- I feel like leftEdge and rightEdge should be handled by the children
      -- but deleting them does not work..
      let leftEdge = insEdge (nodenr, nodenr + 1, 0.5) newGraph
      let rightEdge = insEdge (nodenr, leftmax + 1, 0.5) leftEdge
      let parentEdge = insEdge (parentnr, nodenr, 1.0) rightEdge
      (parentEdge, rightmax)
    EPar e1 e2 -> do
      let (leftGraph, leftmax) = expToGraph' e1 graph (nodenr + 1) nodenr
      let (rightGraph, rightmax) = expToGraph' e2 leftGraph (leftmax + 1) nodenr
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
      let (childGraph, childmax) = expToGraph' e1 graph nodenr parentnr
      let newGraph = insEdge (childmax, nodenr, 1.0) childGraph
      (newGraph, childmax)



expToGraph :: Exp -> PnkGraph
expToGraph expression = do
   -- insert a skip in the beginning as no-op
   -- to prevent self loop at the beginning
  let thegraph = empty  :: PnkGraph
  let seeded = insNode (0, (Skip, 0)) thegraph
  let (graph, _) = expToGraph' expression seeded 1 0
  if expNeedsNormalization expression then toNormalForm graph else graph

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
    [ "Call with one of the following argument combinations:"
    , "  --help           Display this help message."
    , "  -g               Test gfold"
    , "  -i (file)        Run interference on program"
    , "  -c (file)        Compile program to NS-3 C++"
    , "  -p (file)        Attempt to parse program"
    , "  -t (file)        Attempt to parse program and show the resulting tree"
    ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    "-g":_ -> testGfold
    "-p":fs    -> mapM_ (runFile 0 pExp) fs
    "-i":fs  -> interference fs
    "-c":fs    -> createAutomaton fs
    "-t":fs    -> mapM_ (runFile 2 pExp) fs
    fs         -> mapM_ (runFile 2 pExp) fs


prettyPrintSHD :: [(SH, Double)] -> IO ()
prettyPrintSHD [] = return ()
prettyPrintSHD ((sh, d):xs) = do
  putStrLn $ printf "%.2f" (d * 100) ++ "%" ++ " : " ++ show sh
  prettyPrintSHD xs

interference :: [String] ->  IO ()
interference fs = do
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