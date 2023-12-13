module Inference (inferenceExact, inferenceSample) where

import Semantics
import Syntax.Abs

import Prelude hiding (exp)

import qualified Data.Set as Set
import qualified Data.MultiSet as Mset
import Control.Arrow ( Kleisli(runKleisli) )
import Control.Monad (replicateM)
import Data.Maybe (fromMaybe)
import Text.Read (readMaybe)
import Control.Monad.Bayes.Enumerator (Enumerator, enumerator)
import Control.Monad.Bayes.Sampler.Strict (SamplerIO, sampleIOfixed)
import Text.Printf (printf)

import Transformation (transExp)

prettyPrintSingleSHD :: (SH, Double) -> IO ()
prettyPrintSingleSHD (sh, d) = do
  putStrLn $ printf "%.2f" (d * 100) ++ "%" ++ " : " ++ show (Set.toList sh)

prettyPrintSHD :: [(SH, Double)] -> IO ()
prettyPrintSHD [] = return ()
prettyPrintSHD ((sh, d):xs) = do
  prettyPrintSingleSHD (sh, d)
  prettyPrintSHD xs

-- prettyPrintSHI normalises the probabilities and calls prettyPrintSHD
prettyPrintSHI :: Int -> [(SH, Int)] -> IO ()
prettyPrintSHI _ [] = return ()
prettyPrintSHI total ((sh, i):xs) = do
  let d = fromIntegral i / fromIntegral total
  prettyPrintSingleSHD (sh, d)
  prettyPrintSHI total xs

printAsMultiSet :: [SH] -> IO ()
printAsMultiSet input = do
  prettyPrintSHI 1000 $ Mset.toOccurList $ Mset.fromList input


inferenceExact :: Exp -> String ->  IO ()
inferenceExact exp uinput = do
  putStrLn "\nRunning inference."
  let initialSet = Set.fromList input :: SH
  let kleisliArrow = transExp exp ::  Kleisli Enumerator SH SH
  let result = runKleisli kleisliArrow initialSet
  let samples = enumerator result
  putStrLn "Function input:"
  print input
  putStrLn "Function output:"
  prettyPrintSHD samples
  where input = Data.Maybe.fromMaybe [[(0, 0)]] (readMaybe uinput) -- if no input is given, use [[(0, 0)]]

inferenceSample :: Exp -> String ->  IO ()
inferenceSample exp uinput = do
  putStrLn "\nRunning inference."
  let initialSet = Set.fromList input :: SH
  let kleisliArrow = transExp exp ::  Kleisli SamplerIO SH SH
  let result = runKleisli kleisliArrow initialSet
  samples <- sampleIOfixed $ replicateM 10000 result
  putStrLn "Function input:"
  print input
  putStrLn "Function output:"
  printAsMultiSet samples
  where input = Data.Maybe.fromMaybe [[(0, 0)]] (readMaybe uinput) -- if no input is given, use [[(0, 0)]]
