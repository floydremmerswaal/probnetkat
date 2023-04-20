module Main (main) where
  
import Control.Monad (liftM2, replicateM, forM, forM_)
import Control.Monad.IO.Class (liftIO)
import Data.List (sort)
import Control.Monad.Bayes.Class
import Control.Monad.Bayes.Traced
import Control.Monad.Bayes.Weighted
import Control.Monad.Bayes.Inference.SMC as SMC
import Control.Monad.Bayes.Inference.RMSMC as RMSMC
import Control.Monad.Bayes.Traced.Static (Traced)
import Control.Monad.Bayes.Inference.SMC
import Control.Monad.Bayes.Sampler.Strict
import Control.Monad (when)
import Control.Monad.Extra
import Control.Monad.Bayes.Class

import Numeric.Log
import Control.Monad.Bayes.Class

import Data.List (partition)

-- Type definitions
data Field = Field { name :: String, value :: Int } deriving (Show)
type Packet = [Field]
type History = [Packet]

-- assign value to the head package of a history
pAssH :: History -> Field -> History
pAssH [] _ = []
pAssH (x:xs) field = pAss x field:xs

-- assign value to a specific field in a packet
pAss :: Packet -> Field -> Packet
pAss [] _ = []
pAss (x:xs) f = if name x == name f then f:xs else x:pAss xs f

pDupp :: History -> History
pDupp [] = []
pDupp (x:xs) = [x,x] ++ xs

printHead :: History -> IO ()
printHead [] = print "Empty history"
printHead (x:xs) = print x

test = do
    let history = [ [Field "a" 1, Field "b" 2, Field "c" 3]
                  , [Field "a" 4, Field "b" 5, Field "c" 6]
                  , [Field "a" 7, Field "b" 8, Field "c" 9]
                  ]
    printHead []
    printHead history
    let history' = pDupp history
    let history'' = pAssH history' (Field "a" 10)
    printHead history''
    let history''' = pAssH history'' (Field "b" 11)
    printHead history'''
    let history'''' = pAssH history''' (Field "c" 12)
    printHead history''''

-- distribution that has probability p for a1, and 1-p for a2
mProb :: MonadDistribution m => Double -> Field -> Field -> m Field
mProb p a1 a2 = do
  bernoulli p >>= \b -> return $ if b then a1 else a2



main :: IO ()
main = do
  let f1 = Field "a" 1
  let f2 = Field "a" 2

  let nsamples = 1000

  -- sampleIO vs sampleIOfixed (fixed seed or not)
  samples <- sampleIOfixed $ replicateM nsamples (mProb 0.5 f1 f2)

  -- count the number of 1's in samples
  print $ foldl (\acc x -> if name x == "a" && value x == 1 then acc + 1 else acc) 0 samples

  -- count the number of 2's in samples
  print $ foldl (\acc x -> if name x == "a" && value x == 2 then acc + 1 else acc) 0 samples