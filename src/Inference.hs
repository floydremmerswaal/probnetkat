module Inference (inferenceExact, inferenceSample) where

import Semantics
import Syntax.Lex
import Syntax.Par
import Syntax.Abs

import qualified Data.Set as Set
import qualified Data.MultiSet as Mset
import Control.Arrow
import Control.Monad (replicateM)
import Control.Monad.Bayes.Class
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Control.Monad.Bayes.Enumerator (Enumerator, enumerator)
import Control.Monad.Bayes.Sampler.Strict (SamplerIO, sampleIOfixed)
import System.Exit ( exitFailure )
import Text.Printf (printf)

import Transformation (transExp)

itemOf :: Int -> [a] -> Maybe a; x `itemOf` xs = let xslen = length xs in if ((abs x) > xslen) then Nothing else Just (xs !! (x `mod` xslen))  

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
  prettyPrintSHI 10000 $ Mset.toOccurList $ Mset.fromList input

inferenceExact :: [String] ->  IO ()
inferenceExact fs = do
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
      putStrLn "Function input:"
      print input
      putStrLn "Function output:"
      prettyPrintSHD samples
  where input = Data.Maybe.fromMaybe [[(0, 0)]] (readMaybe (fromMaybe "[[(0,0)]]" (itemOf 1 fs))) -- if no input is given, use [[(0, 0)]]


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
      putStrLn "Function input:"
      print input
      putStrLn "Function output:"
      printAsMultiSet samples
  where input = Data.Maybe.fromMaybe [[(0, 0)]] (readMaybe (fromMaybe "[[(0,0)]]" (itemOf 1 fs))) -- if no input is given, use [[(0, 0)]]