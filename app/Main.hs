module Main (Main.main) where
    -- main function


import System.IO ( stdin, hGetContents )
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure, exitSuccess )
import Control.Monad (when)

import Syntax.Lex
import Syntax.Par
import Syntax.Print
import Syntax.Abs
import Semantics


import Syntax.ErrM

type ParseFun a = [Token] -> Err a

myLLexer = myLexer

type Verbosity = Int

putStrV :: Verbosity -> String -> IO ()
putStrV v s = when (v > 1) $ putStrLn s

runFile :: (Print a, Show a) => Verbosity -> ParseFun a -> FilePath -> IO ()
runFile v p f = putStrLn f >> readFile f >>= run v p

run :: (Print a, Show a) => Verbosity -> ParseFun a -> String -> IO ()
run v p s = let ts = myLLexer s in case p ts of
           Bad s    -> do putStrLn "\nParse              Failed...\n"
                          putStrV v "Tokens:"
                          putStrV v $ show ts
                          putStrLn s
                          exitFailure
           Ok  tree -> do putStrLn "\nParse Successful!"
                          showTree v tree

parse :: (Print a, Show a) => Verbosity -> ParseFun a -> String -> IO a
parse v p s = let ts = myLLexer s in case p ts of
           Bad s    -> do putStrLn "\nParse              Failed...\n"
                          putStrV v "Tokens:"
                          putStrV v $ show ts
                          putStrLn s
                          exitFailure
           Ok  tree -> do putStrLn "\nParse Successful!"
                          return tree

showTree :: (Show a, Print a) => Int -> a -> IO ()
showTree v tree
 = do
      putStrV v $ "\n[Abstract Syntax]\n\n" ++ show tree
      putStrV v $ "\n[Linearized tree]\n\n" ++ printTree tree

usage :: IO ()
usage = do
  putStrLn $ unlines
    [ "usage: Call with one of the following argument combinations:"
    , "  --help                                 Display this help message."
    , "  (no arguments)                         Parse stdin verbosely."
    , "  (files)                                Parse content of files verbosely."
    , "  -s (files)                             Silent mode. Parse content of files silently."
    , "  -e (semantics) (environment) (files)   Parse content of files and evaluate with given"
      ++ "semantics (d - denotationational, b - big-step, s - small step, s1 - one small step, "
      ++ "l - small step with left-first evaluation, l1 - one left-first step) and environment"
    ]
  exitFailure

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--help"] -> usage
    [] -> getContents >>= run 2 pExp
    "-s":fs -> mapM_ (runFile 0 pExp) fs