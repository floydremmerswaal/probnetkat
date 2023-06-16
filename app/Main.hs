module Main (Main.main) where
    -- main function

import Prelude hiding (drop, seq)

import System.Environment ( getArgs )
import System.Exit ( exitFailure, exitSuccess )
import Control.Monad (when)
import qualified Data.Set as Set
import Data.Set (Set)

import Syntax.Lex
import Syntax.Par
import Syntax.Print
import Syntax.Abs
import Semantics
import Transformation

import Control.Monad.Bayes.Class
import Control.Arrow

import Control.Monad.Bayes.Enumerator

import Control.Monad.Bayes.Sampler.Strict
import Control.Monad.Bayes.Enumerator

import Control.Monad (replicateM)

import Syntax.ErrM

type ParseFun a = [Token] -> Err a

myLLexer = myLexer

type Verbosity = Int


putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

runFileAndIO :: (Print a, Show a) => Verbosity -> ParseFun a -> FilePath -> IO ()
runFileAndIO v p f = do
  runFile v p f
  putStrLn "Starting interactive session..."
  --interactiveSession


interactiveSession :: IO ()
interactiveSession = do 
  putStr ""
  line <- getLine
  if line == "quit" 
    then putStrLn "Bye"
    else do
      case line of 
          "" -> putStrLn "success!"
          _      -> putStrLn "unknown command"
      interactiveSession 

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
    ]

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    []         -> getContents >>= run 2 pExp
    "-s":fs    -> mapM_ (runFile 0 pExp) fs
    "--test":fs  -> testF fs
    fs         -> mapM_ (runFileAndIO 2 pExp) fs

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
      let kleisliArrow = transExp tree ::  Kleisli Enumerator SH SH
      let sw = Field (Ident "sw") 1
      let sh = Set.fromList [[[sw]]]
      let empty = Set.empty
      let result1 = runKleisli kleisliArrow sh
      let result2 = runKleisli kleisliArrow empty
      let samples1 = enumerator result1
      let samples2 = enumerator result2
      print samples1 -- this should output sw = 1 and pt = 1
      print samples2 -- if adding fields if not present works, this should be the same
      putStrLn "Function is defined"