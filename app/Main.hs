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


-- -- create list of tokens from file
-- fileToExp :: String -> Exp
-- fileToExp s = do 
--     let ts = myLexer s
--     case pExp ts of
--       Left _ -> do
--         ESkip -- default to the skip expression if we error out. could be done better but oh well
--       Right tree -> do
--         tree

outputCppInstr :: Int -> Int -> Exp -> String
outputCppInstr i j (EAssSw n) = "int instr" ++ show i ++ " = addNode(SW, " ++ show n ++ ", " ++ show j ++ ");"
outputCppInstr i j (EAssPt n) = "int instr" ++ show i ++ " = addNode(PT, " ++ show n ++ ", " ++ show j ++ ");"
outputCppInstr i j (ESwEq n) = "int instr" ++ show i ++ " = addNode(TESTSW, " ++ show n ++ ", " ++ show j ++ ");"
outputCppInstr i j (EPtEq n) = "int instr" ++ show i ++ " = addNode(TESTPT, " ++ show n ++ ", " ++ show j ++ ");"
outputCppInstr i j (ESwNEq n) = "int instr" ++ show i ++ " = addNode(TESTSW, " ++ show n ++ ", " ++ show j ++ ");"
outputCppInstr i j (EPtNEq n) = "int instr" ++ show i ++ " = addNode(TESTPT, " ++ show n ++ ", " ++ show j ++ ");"
outputCppInstr i j EDup = "int instr" ++ show i ++ " = addNode(DUP, 0, " ++ show j ++ ");"
outputCppInstr i j ESkip = "int instr" ++ show i ++ " = addNode(SKIP, 0, " ++ show j ++ ");"
outputCppInstr i j EDrop = "int instr" ++ show i ++ " = addNode(DROP, 0, " ++ show j ++ ");"
outputCppInstr i j (ESeq e1 e2) =  outputCppInstr i (j+1) e1 ++ "\n" ++ outputCppInstr (j+1) j e2
outputCppInstr _ _ (EprobD _ _) = "might be deleted"
outputCppInstr i j (EProb e1 d e2) = outputCppInstr i (j+1) e1 ++ "\n" ++ outputCppInstr (j+1) j e2
outputCppInstr i j (Epar e1 e2) = outputCppInstr i (j+1) e1 ++ "\n" ++ outputCppInstr (j+1) j e2
outputCppInstr i j (EKleene e1) = outputCppInstr i (j+1) e1 ++ "\n" ++ outputCppInstr (j+1) j e1


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
      putStrLn "\nParse Successful!"
      showTree 2 tree
      -- traverse the tree and print the c++ code
      putStrLn "C++ code:"
      putStrLn $ outputCppInstr tree