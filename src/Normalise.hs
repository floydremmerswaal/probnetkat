module Normalise where

import Syntax.Lex
import Syntax.Abs

import Automaton (expToGraph, Inst ( AssSw , AssPt , TestSw , TestPt , Dup , Par , Prob , Drop, Skip), InstNode, PnkGraph)

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
-- import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Tree (Gr)
import Data.Graph.Inductive.Query.DFS

import Data.Graph.Inductive.Dot (fglToDot, showDot)

import Control.Monad.State

import Debug.Trace

type PnkContext = Context InstNode Double


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

simpleTest :: [LNode InstNode]
simpleTest = 
  [
    (0, (Par, 0.0))
  , (1, (Prob, 0.3))
  , (4, (Prob, 0.6))
  , (2, (AssSw, 3.0))
  , (3, (AssPt, 4.0))
  , (5, (AssPt, 5.0))
  , (6, (AssSw, 6.0))
  ]

simpleTestEdges :: [LEdge Double] 
simpleTestEdges = 
  [
    (0, 1, 1.0)
  , (0, 4, 1.0)
  , (1, 2, 0.3)
  , (1, 3, 0.7)
  , (4, 5, 0.6)
  , (4, 6, 0.4)
  , (2, 0, 1.0)
  , (3, 0, 1.0)
  , (5, 0, 1.0)
  , (6, 0, 1.0)
  ]

test1 :: PnkGraph
test1 = mkGraph testNodes1 testEdges1

testSimple :: PnkGraph
testSimple = mkGraph simpleTest simpleTestEdges

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
-- r is usually 0, the root of the automaton.
automToTree :: Node -> PnkGraph -> SpTree
automToTree r =
  head . xdffWith suc' (\c@(inAdj, n, l, outAdj) ->
                          trace ("Adding context " ++ show c ++ " to tree.")
                          ((inAdj, n, l), fmap fst outAdj)) [r]

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
normStep ((inAdj, n, instr), ws)    [] = do -- leaf node
  n' <- genNode
  setOrigin n n'
  pure $ Tr.Node ((inAdj, n', instr), ws) []
normStep (ctx@(_, _, (Par, _)), ws) ts = mergeDown ctx (partition isProb $ zip ws ts) -- par node
normStep ((inAdj, n, instr), ws)    ts =  do -- the rest
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

testGfold :: IO ()
testGfold = do
  --let t = automToTree 0 test1
  let t = automToTree 0 testSimple
  putStrLn $ drawTree $ fmap show t
  let t' = evalState (normTree t >>= updateInEdges) normInit
  putStrLn $ drawTree $ fmap show t'
  let g' = treeToAutom t'
  -- write to file
  writeGraphToFile "test_test.dot" testSimple
  writeGraphToFile "test_gfold.dot" g'
  getGraphNodes testSimple
  getGraphNodes g'


testNormalization :: Exp -> IO ()
testNormalization expression = do
  let graph = expToGraph expression
  -- let graph = mkGraph (labNodes graph') (labEdges graph')
  let t = automToTree 0 graph
  let p_t = automToTree 0 testSimple
  putStrLn $ "match expr: " ++ show (match 0 graph)
  putStrLn $ "match tree: " ++ show (match 0 testSimple)
  putStrLn "expression to graph nodes and edges:"
  getGraphNodes graph
  putStrLn "testSimple to graph nodes and edges:"
  getGraphNodes testSimple
  putStrLn "expression to tree:"
  putStrLn $ drawTree $ fmap show t
  putStrLn "testSimple to tree:"
  putStrLn $ drawTree $ fmap show p_t
  let t' = evalState (normTree t >>= updateInEdges) normInit
  let p_t' = evalState (normTree p_t >>= updateInEdges) normInit
  putStrLn $ drawTree $ fmap show t'
  putStrLn $ drawTree $ fmap show p_t'
  let g' = treeToAutom t'
  let p_g' = treeToAutom p_t'
  -- write to file
  -- writeCppFile $ graphToInstructionList graph
  writeGraphToFile "comp_test.dot" graph
  writeGraphToFile "comp_gfold.dot" g'
  writeGraphToFile "pcomp_test.dot" testSimple
  writeGraphToFile "pcomp_gfold.dot" p_g'

  getGraphNodes g' 
  getGraphNodes p_g'
  


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

-- type of Graph is Gr a b with a the type for the nodes and b the type for the edges
testGraph :: Exp -> IO ()
testGraph expression = do
  putStr "testGraph\n"
  -- let graph = expToGraph expression
  testNormalization expression
  -- prettyPrint graph
  -- writeGraphToFile "automaton.dot" graph
  -- getGraphNodes graph
  -- putStr $ graphToInstructionList graph