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
import Control.Monad.Bayes.Sampler.Strict
import Control.Monad.State
import Control.Monad.Reader

import Text.Read (readMaybe)

import Data.Maybe (fromJust)
import qualified Data.IntMap.Strict as Map
import Data.IntMap.Strict (IntMap)
import Data.List (partition)

import Data.Maybe (fromMaybe)

import qualified Data.MultiSet as Mset

import Syntax.ErrM

import Data.Tree as Tr
import Data.Tree.Pretty
import Data.Tuple.Extra (snd3, thd3)


import Data.Graph.Inductive.Graph -- (Context, Node, prettyPrint, insNode, insEdge, empty, nodes, edges, insNodes, Graph (mkGraph), labNodes, labEdges, LNode, LEdge, delEdges, delNodes, insEdges, (&))
import Data.Graph.Inductive.Basic (gfold)
import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Query.DFS

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
orderNormalise :: PnkContext -> [Node]
orderNormalise ctx = do
  let (_, node, _, post) = ctx
  let nextnodes = map snd post
  trace ("Visiting node: " ++ show node ++ " with next nodes: " ++ show nextnodes ++ " context: " ++  show ctx) nextnodes

-- -- da :: (Context a b -> c -> d) -- depth aggregation	
-- -- The function takes a context, and combines it with the accumulated value (c),
-- -- returning a possibly different type (d).
-- da :: PnkContext -> PnkGraph -> PnkGraph -- for now, merge the context into the graph
-- da context g =
--   trace ("Depth aggregation for graph: " ++ show g ++ " context: " ++ show context) (context & g)

-- -- Maybe d -> c -> c
-- combineGraphs :: Maybe PnkGraph -> PnkGraph -> PnkGraph -- insert the nodes and edges of the maybeGraph into the graph
-- combineGraphs maybeGraph graph =
--   case maybeGraph of
--     Nothing -> graph -- Nothing case shouldn't happen, but we have to account for it
--     Just g -> do 
--       let ns = labNodes g
--       let es = labEdges g
--       trace ("Adding graph " ++ show g ++ "\ngraph = " ++ show graph) $ insEdges es $ insNodes ns graph

-- -- ba :: (Maybe d -> c -> c, c)	-- breadth/level aggregation
-- ba :: (Maybe PnkGraph -> PnkGraph -> PnkGraph, PnkGraph)
-- ba = (trace "Breadth aggregation" combineGraphs, empty)

testNodes1 :: [LNode InstNode]
testNodes1 =
  [ (0, (AssSw, 1.0))
  , (1, (AssPt, 2.0))
  , (2, (Par, 0.0))
  , (3, (Prob, 0.5))
  , (4, (Prob, 0.5))
  , (5, (Skip, 0.0))
  , (6, (Skip, 0.0))
  , (7, (Skip, 0.0))
  , (8, (Skip, 0.0))
  ]

testEdges1 :: [LEdge Double]
testEdges1 =
  [ (0, 1, 1.0)
  , (1, 2, 1.0)
  , (2, 3, 1.0)
  , (2, 4, 1.0)
  , (3, 5, 0.4)
  , (3, 6, 0.6)
  , (4, 7, 0.3)
  , (4, 8, 0.7)
  , (8, 0, 1.0)
  , (2, 0, 1.0)
  ]

test1 :: PnkGraph
test1 = mkGraph testNodes1 testEdges1

test1Tree :: Tree PnkContext
test1Tree = head $ xdffWith suc' id [0] test1


-- | Lifts gfold to monadic operations
-- (and eliminates Maybe from breadth aggregation, or is that too dangerous?)
gfoldM :: (Graph gr, Monad m) =>
  (Context a b -> [Node])      -- ^ direction of fold
  -> (Context a b -> c -> m d) -- ^ depth aggregation
  -> (d -> c -> m c, c)        -- ^ breadth/level aggregation
  -> [Node]
  -> gr a b
  -> m c
gfoldM di da (ba, b0) = gfold di daM (baM, pure b0)
  where
    -- daM :: Context a b -> m c -> m d
    daM ctx = (=<<) (da ctx)
    -- baM :: Maybe (m d) -> m c -> m c
    baM maybeMD = \mc -> fromJust maybeMD >>= \d -> mc >>= ba d

-- | State of automaton normalisation
data NormalisationState = NormalisationState
  { nextNode :: Int           -- ^ Next node name to be generated
  , origin :: IntMap [Node]   -- ^ Tracks the nodes that have been generated for an old node
  }

-- | Initial state for normalisation
normInit :: NormalisationState
normInit = NormalisationState 0 Map.empty

-- | Monad used for automaton normalisation
type NormM = State NormalisationState

-- | Monadic fold for trees
foldTreeM :: Monad m => (a -> [b] -> m b) -> Tree a -> m b
foldTreeM f = foldTree $ \a ml -> sequence ml >>= f a

-- | Generate a fresh node name
genNode :: NormM Node
genNode = state $ \s -> (nextNode s, s{nextNode = nextNode s + 1})

-- | Saves the reference from the old to the new node name
setOrigin :: Node -> Node -> NormM ()
setOrigin orig new = modify $ \s -> s{ origin = Map.insertWith (++) orig [new] (origin s) }

-- | Retrieves the new names that were generated for a node n
getMapped :: Node -> NormM [Node]
getMapped n = gets (Map.findWithDefault [] n . origin)

-- | Context of only incoming edges.
type InCtx = (Adj Double, Node, InstNode)

-- | Label used in the spanning tree of an automaton.
-- The list in the second components contains the weights for the children of a node.
type SpLab = (InCtx, [Double])

-- | Spanning tree of an automaton.
-- Careful: the weights of edges are stored in a list in the parent node. This is not the safest
-- option but otherwise a tree type with labels on the edges is needed.
type SpTree = Tree SpLab

-- | Label of the root in a spanning tree
rootLabel' :: SpTree -> InCtx
rootLabel' = fst . rootLabel

-- | Returns the children of the root of a given tree with the edge weights.
subForest' :: SpTree -> [(Double, SpTree)]
subForest' t = zip (snd $ rootLabel t) $ subForest t

-- | Computes the spanning tree of the given automaton from the designated root node r.
-- It is assumed that r is in the given graph, otherwise the function leads to an unrecoverable
-- error.
automToTree :: Node -> PnkGraph -> SpTree
automToTree r =
  head . xdffWith suc' (\(inAdj, n, l, outAdj) -> ((inAdj, n, l), fmap fst outAdj)) [r]

-- | Merges a non-deterministic choice down into the given forest.
-- The first list of children must be the probabilistic nodes and the other
-- list the remaining ones.
mergeDown :: InCtx -> ([(Double, SpTree)], [(Double, SpTree)]) -> NormM (SpTree)
mergeDown (inAdj, n, instr) ([], remChildren) =  do
  n' <- genNode
  setOrigin n n'
  let (ws, ch) = unzip remChildren
  pure $ Tr.Node ((inAdj, n', instr), ws) ch
mergeDown ctx ((w, probTree) : probTrees, remChildren) = do
  let (ws, f) = unzip $ subForest' probTree
  let ts1 = zip (repeat w) f
  ts <- mapM (\probTreeCh -> mergeDown ctx (probTrees, remChildren ++ [probTreeCh])) ts1
  let (inAdj, n, instr) = rootLabel' probTree
  n' <- genNode
  setOrigin n n'
  pure $ Tr.Node ((inAdj, n', instr), ws) ts

-- | Tests if the edge points to a tree that has a probabilistic choice at the root.
isProb :: (Double, SpTree) -> Bool
isProb (_, t) = case thd3 (rootLabel' t) of
  (Prob, _) -> True
  _         -> False

-- | Implements on step of the normalisation procedure.
-- Essentially checks if we are currently at a non-deterministic choice nodes and
-- merges that down the tree.
normStep :: SpLab -> [SpTree] -> NormM (SpTree)
normStep ((inAdj, n, instr), ws)    [] = do
  n' <- genNode
  setOrigin n n'
  pure $ Tr.Node ((inAdj, n', instr), ws) []
normStep (ctx@(_, _, (Par, _)), ws) ts = mergeDown ctx (partition isProb $ zip ws ts)
normStep ((inAdj, n, instr), ws)    ts =  do
  n' <- genNode
  setOrigin n n'
  pure $ Tr.Node ((inAdj, n', instr), ws) ts

-- | Normalise spanning tree of automaton by distributing probabilistic choice up over
-- non-deterministic choice.
-- Edges that are not in the tree but in the labels are considered recursive edges and are not
-- touched.
normTree :: SpTree -> NormM (SpTree)
normTree = foldTreeM normStep

-- | Transforms a tree back into an automaton.
treeToAutom :: SpTree -> PnkGraph
treeToAutom = uncurry insEdges . fmap buildGr . foldTree merge
  where
    merge :: SpLab -> [([LEdge Double], [PnkContext])] -> ([LEdge Double], [PnkContext])
    merge ((inAdj, n, l), ws) a =
      let (es, ch) = sequence a :: ([LEdge Double], [[PnkContext]])
          inEdges = fmap (\(w, k) -> (k, n, w)) inAdj
          outAdj = zip ws $ fmap (node' . head) ch
      in (inEdges ++ es, ([], n, l, outAdj) : join ch) -- foldr merge1 ([], empty)

-- | Given a tree of in-contexts where the incoming edges are referring to nodes before the normTree
-- has been run on the tree, change the node references to those generated by normTree in the state.
updateInEdges :: SpTree -> NormM (SpTree)
updateInEdges = mapM upd
  where
    upd :: SpLab -> NormM SpLab
    upd ((inAdj, n, instr), ws) = mapM updAdj inAdj >>= \inAdjs -> pure ((join inAdjs, n, instr), ws)

    updAdj :: (Double, Node) -> NormM [(Double, Node)]
    updAdj (w, k) = fmap ((,) w) <$> getMapped k

-- depthAggrNormalise :: Context InstNode Double -> PnkGraph -> NormM NormInterGraph
-- depthAggrNormalise ctx@(ie, n, l, oe) g = do
--   n' <- genNode
--   -- setOrigin n n'
--   -- (mappedO, unmappedO) <- getMapped oe
--   trace ("Depth aggregation for graph: " ++ show g ++ " context: " ++ show ctx) (pure $ ctx & g)

-- breadthAggrNormalise :: NormInterGraph -> PnkGraph -> NormM PnkGraph
-- breadthAggrNormalise g graph =
--   let ns = labNodes g
--       es = labEdges g
--   in trace ("Adding graph " ++ show g ++ "\ngraph = " ++ show graph) (pure $ insEdges es $ insNodes ns graph)

-- normalise :: PnkGraph -> Node -> PnkGraph
-- normalise g r =
--   evalState (gfoldM orderNormalise depthAggrNormalise (breadthAggrNormalise, empty) [r] g) normInit

-- normalise :: PnkGraph -> Node -> PnkGraph
-- normalise g r = _

testGfold :: IO ()
testGfold = do
  let t = automToTree 0 test1
  putStrLn $ drawTree $ fmap show t
  let t' = evalState (normTree t >>= updateInEdges) normInit
  putStrLn $ drawTree $ fmap show t'
  let g' = treeToAutom t'
  -- write to file
  writeGraphToFile "test.dot" test1
  writeGraphToFile "gfold.dot" g'
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
    , "  --help               Display this help message."
    , "  -g                   Test gfold"
    , "  -ie (file) (input)  Run exact inference on program"
    , "  -is (file) (input)  Run sample inference on program"
    , "  -c (file)            Compile program to NS-3 C++"
    , "  -p (file)            Attempt to parse program"
    , "  -t (file)            Attempt to parse program and show the resulting tree"
    ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    "-g":_ -> testGfold
    "-p":fs    -> mapM_ (runFile 0 pExp) fs
    "-ie":fs  -> inference fs
    "-is":fs  -> inferenceSample fs
    "-c":fs    -> createAutomaton fs
    "-t":fs    -> mapM_ (runFile 2 pExp) fs
    fs         -> mapM_ (runFile 2 pExp) fs


prettyPrintSingleSHD :: (SH, Double) -> IO ()
prettyPrintSingleSHD (sh, d) = do
  putStrLn $ printf "%.2f" (d * 100) ++ "%" ++ " : " ++ show (Set.toList sh)

prettyPrintSHD :: [(SH, Double)] -> IO ()
prettyPrintSHD [] = return ()
prettyPrintSHD ((sh, d):xs) = do
  prettyPrintSingleSHD (sh, d)
  prettyPrintSHD xs

-- prettyPrintSHI normalizes the probabilities and calls prettyPrintSHD
prettyPrintSHI :: Int -> [(SH, Int)] -> IO ()
prettyPrintSHI _ [] = return ()
prettyPrintSHI total ((sh, i):xs) = do
  let d = fromIntegral i / fromIntegral total
  prettyPrintSingleSHD (sh, d)
  prettyPrintSHI total xs

-- we could make a 'pure' version of this function that returns the result of the runKleisli
-- which could then be sampled from
-- input is now essentially a dirac delta on a list of samples, but we could make it a distribution

printOccurList :: (Show a) => [(a, Int)] -> IO ()
printOccurList [] = return ()
printOccurList ((x, y):xs) = do
  putStrLn $ show x ++ " : " ++ show y
  printOccurList xs

printAsMultiSet :: [SH] -> IO ()
printAsMultiSet input = do
  let multiset = Mset.fromList input
  let occurlist = Mset.toOccurList multiset

  print multiset
  prettyPrintSHI 10000 occurlist
  

inference :: [String] ->  IO ()
inference fs = do
  s <- readFile (head fs)
  let ts = myLexer s
  case pExp ts of
    Left err -> do
      putStrLn "\nParse Failed...\n"
      putStrLn err
      exitFailure
    Right tree -> do
      putStrLn "\nRunning inference."
      print tree
      let initialSet = Set.fromList input :: SH
      let kleisliArrow = transExp tree ::  Kleisli Enumerator SH SH
      let result = runKleisli kleisliArrow initialSet
      let samples = enumerator result
      putStrLn "Function output:"
      prettyPrintSHD samples
  where input = Data.Maybe.fromMaybe [[(0, 0)]] (readMaybe (head (tail fs))) -- if no input is given, use [[(0, 0)]]

inferenceSample :: [String] ->  IO ()
inferenceSample fs = do
  s <- readFile (head fs)
  let ts = myLexer s
  case pExp ts of
    Left err -> do
      putStrLn "\nParse Failed...\n"
      putStrLn err
      exitFailure
    Right tree -> do
      putStrLn "\n Sampling inference."
      print tree
      let initialSet = Set.fromList input :: SH
      let kleisliArrow = transExp tree ::  Kleisli SamplerIO SH SH
      let result = runKleisli kleisliArrow initialSet
      samples <- sampleIOfixed $ replicateM 10000 result
      putStrLn "Function output:"
      printAsMultiSet samples
      -- prettyPrintSHD samples
      -- print out the samples
      
  where input = Data.Maybe.fromMaybe [[(0, 0)]] (readMaybe (head (tail fs))) -- if no input is given, use [[(0, 0)]]




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
  let newRightTree = appendToLeaves (head rightTree) someSubTree
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