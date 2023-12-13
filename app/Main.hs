module Main (Main.main) where
    -- main function

import Prelude hiding (drop, seq)

import System.Environment ( getArgs )
import System.Exit ( exitFailure )
import Syntax.Lex
import Syntax.Par
import Syntax.Abs
import Syntax.Print
import Inference (inferenceExact, inferenceSample)
import Automaton (createAutomatonIO)
import Normalise

import Control.Monad.State

import Syntax.ErrM

type ParseFun a = [Token] -> Err a

type Verbosity = Int


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
    , "  -i (file) (input)    Run exact inference on program"
    , "  -s (file) (input)    Run sample inference on program"
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
    "-i":fs  -> parseAndForwardArg fs inferenceExact
    "-s":fs  -> parseAndForwardArg fs inferenceSample
    "-c":fs    -> parseAndForward fs createAutomatonIO
    "-t":fs    -> mapM_ (runFile 2 pExp) fs
    fs         -> mapM_ (runFile 2 pExp) fs

parseAndForwardArg :: [String] -> (Exp -> String -> IO()) -> IO ()
parseAndForwardArg fs f = do
  s <- readFile (head fs)
  let ts = myLexer s
  case pExp ts of
    Left err -> do
      putStrLn "\nParse Failed...\n"
      putStrLn err
      exitFailure
    Right tree -> do
      putStrLn "\nParse successful!"
      showTree 2 tree
      f tree (head (tail fs))
  
parseAndForward :: [String] -> (Exp -> IO()) -> IO ()
parseAndForward fs f = do
  s <- readFile (head fs)
  let ts = myLexer s
  case pExp ts of
    Left err -> do
      putStrLn "\nParse Failed...\n"
      putStrLn err
      exitFailure
    Right tree -> do
      putStrLn "\nParse successful!"
      showTree 2 tree
      f tree