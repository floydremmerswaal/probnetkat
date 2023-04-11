module Main (main) where

import Prelude 
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Enumerator
import Control.Monad.Bayes.Weighted
import Control.Monad.Bayes.Sampler.Strict
import Control.Monad

-- import Lib

random :: Distribution Double

example :: MonadMeasure m => m Double
example = do
  bool <- bernoulli 0.5
  if bool then random else normal 0 1

model :: MonadMeasure m => m Bool
model = do
  x <- bernoulli 0.5
  y <- bernoulli 0.3
  condition (x || y)
  return x

enumerator model

main :: IO ()
main = do
    example
    putStrLn "someFunc"