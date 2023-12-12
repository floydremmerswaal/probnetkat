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
import Inference (inferenceExact, inferenceSample)
import Automaton (createAutomatonIO)
import Normalise

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
-- import Data.Graph.Inductive.PatriciaTree (Gr)
import Data.Graph.Inductive.Tree (Gr)
import Data.Graph.Inductive.Query.DFS

import Data.Graph.Inductive.Dot (fglToDot, showDot)

import Debug.Trace

type ParseFun a = [Token] -> Err a

type Verbosity = Int

data Inst = AssSw | AssPt | TestSw | TestPt | Dup | Par | Prob | Drop | Skip deriving Show
type InstNode = (Inst, Double)

type PnkGraph = Gr InstNode Double
type PnkContext = Context InstNode Double

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
    , "  -ie (file) (input)   Run exact inference on program"
    , "  -is (file) (input)   Run sample inference on program"
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
    "-ie":fs  -> inferenceExact fs
    "-is":fs  -> inferenceSample fs
    "-c":fs    -> createAutomatonIO fs
    "-t":fs    -> mapM_ (runFile 2 pExp) fs
    fs         -> mapM_ (runFile 2 pExp) fs

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
