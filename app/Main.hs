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
import Text.Read.Lex (Number)

import Data.Tree

type ParseFun a = [Token] -> Err a

type Verbosity = Int


putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

runFileAndIO :: (Print a, Show a) => Verbosity -> ParseFun a -> FilePath -> IO ()
runFileAndIO v p f = do
  runFile v p f
  putStrLn "Starting interactive session..."
  --interactiveSession


-- interactiveSession :: IO ()
-- interactiveSession = do 
--   putStr ""
--   line <- getLine
--   if line == "quit" 
--     then putStrLn "Bye"
--     else do
--       case line of 
--           "" -> putStrLn "success!"
--           _      -> putStrLn "unknown command"
--       interactiveSession 

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
    []         -> getContents >>= run 2 pExp
    "-s":fs    -> mapM_ (runFile 0 pExp) fs
    "--test":fs  -> testF fs
    "--auto":fs -> createAutomaton fs
    fs         -> mapM_ (runFileAndIO 2 pExp) fs


prettyPrint :: [(SH, Double)] -> IO ()
prettyPrint [] = return ()
prettyPrint ((sh, d):xs) = do
  putStrLn $ printf "%.2f" (d * 100) ++ "%" ++ " : " ++ show sh
  prettyPrint xs

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
      prettyPrint samples

dfsInit :: Exp -> IO Int
dfsInit = dfsExp 0 0

dfsExp :: Int -> Int ->  Exp -> IO Int
dfsExp nodenr parentnr expression = do
  -- putStrLn $ show i 
  case expression of
    EAssSw arg -> do
      putStrLn $ "int node" ++ show nodenr ++ " = addNode(" ++ show parentnr ++ ",  SW, " ++ show arg ++ ", 0.0);"
      return nodenr
    EAssPt arg -> do
      putStrLn $ "int node" ++ show nodenr ++ " = addNode(" ++ show parentnr ++ ",  PT, " ++ show arg ++ ", 0.0);"
      return nodenr
    ESwEq arg -> do
      putStrLn $ "int node" ++ show nodenr ++ " = addNode(" ++ show parentnr ++ ",  TESTSW, " ++ show arg ++ ", 0.0);"
      return nodenr
    EPtEq arg -> do
      putStrLn $ "int node" ++ show nodenr ++ " = addNode(" ++ show parentnr ++ ",  TESTPT, " ++ show arg ++ ", 0.0);"
      return nodenr
    ESwNEq arg -> do
      putStrLn $ "int node" ++ show nodenr ++ " = addNode(" ++ show parentnr ++ ",  TESTSWNEG, " ++ show arg ++ ", 0.0);"
      return nodenr
    EPtNEq arg -> do
      putStrLn $ "int node" ++ show nodenr ++ " = addNode(" ++ show parentnr ++ ",  TESTPTNEG, " ++ show arg ++ ", 0.0);"
      return nodenr
    EDup -> do
      putStrLn $ "int node" ++ show nodenr ++ " = addNode(" ++ show parentnr ++ ",  DUP, 0, 0.0);"
      return nodenr
    ESkip -> do
      putStrLn $ "int node" ++ show nodenr ++ " = addNode(" ++ show parentnr ++ ",  SKIP, 0, 0.0);"
      return nodenr
    EDrop -> do
      putStrLn $ "int node" ++ show nodenr ++ " = addNode(" ++ show parentnr ++ ",  DROP, 0, 0.0);"
      return nodenr
    ESeq e1 e2 -> do
      leftmax <- dfsExp nodenr parentnr e1
      dfsExp (leftmax + 1) leftmax e2
    EProbD e1 e2 -> do
      putStrLn $ "int node" ++ show nodenr ++ " = addNode(" ++ show parentnr ++ ",  PROB, 0, 0.5);"
      leftmax <- dfsExp (nodenr + 1) nodenr  e1
      dfsExp (leftmax + 1) nodenr e2
    EProb e1 d e2 -> do
      putStrLn $ "int node" ++ show nodenr ++ " = addNode(" ++ show parentnr ++ ",  PROB, 0, " ++ show d ++ ");"
      leftmax <- dfsExp (nodenr + 1) nodenr  e1
      dfsExp (leftmax + 1) nodenr e2
    EPar e1 e2 -> do
      putStrLn $ "int node" ++ show nodenr ++ " = addNode(" ++ show parentnr ++ ",  PAR, 0, 0.0);"
      leftmax <- dfsExp (nodenr + 1) nodenr  e1
      dfsExp (leftmax + 1) nodenr e2
    EKleene e1 -> do
      putStrLn $ "int node" ++ show nodenr ++ " = addNode(" ++ show parentnr ++ ",  KLEENESTART, 0, 0.0);"
      childmax <- dfsExp (nodenr + 1) nodenr e1
      putStrLn $ "int node" ++ show (childmax + 1) ++ " = addNode(" ++ show parentnr ++ ",  KLEENESTOP, 0, 0.0);"
      return (childmax + 1)


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
      putStrLn "\nParse Successful!\n"
      showTree 2 tree
      -- traverse the tree and print the c++ code
      putStrLn "DFS:"
      _ <- dfsInit tree
      putStrLn "done"


data Inst = Ass | Pt | TestSw | TestPt | Dup | Par | Prob | Drop | Skip | KleeneStart | KleeneStop
type InstNode = (Inst, Double)


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


-- we take in a program, and return a tree of nodes
expToTree :: Exp -> Tree InstNode 
expToTree expression = do
  case expression of
    EAssPt arg -> Node (Pt, fromInteger arg) []
    EAssSw arg -> Node (Ass, fromInteger arg) []
    ESwEq arg -> Node (TestSw, fromInteger arg) []
    EPtEq arg -> Node (TestPt, fromInteger arg) []    
    ESwNEq arg -> Node (TestSw,fromInteger arg) [] -- negation not yet implemented!! TODO decide if we should remove or not
    EPtNEq arg -> Node (TestPt, fromInteger arg) []
    EDup -> Node (Dup, 0) []
    ESkip -> Node (Skip, 0) []
    EDrop -> Node (Drop, 0) []
    ESeq e1 e2 -> do
      leftTree <- expToTree e1
      rightTree <- expToTree e2
      return $ appendToLeaves leftTree rightTree
    EProb e1 d e2 -> do
      leftTree <- expToTree e1
      rightTree <- expToTree e2
      Node (Prob, d) [leftTree, rightTree] 
    EProbD e1 e2 -> do
      leftTree <- expToTree e1
      rightTree <- expToTree e2
      Node (Prob, 0.5) [leftTree, rightTree] 
    EPar e1 e2 -> do
      leftTree <- expToTree e1
      rightTree <- expToTree e2
      Node (Par, 0) [leftTree, rightTree]
    EKleene e1 -> do
      childTree <- expToTree e1
      fixedChildTree <- appendToLeaves childTree (Node (KleeneStop, 0) []) 
      Node (KleeneStart, 0) [fixedChildTree]