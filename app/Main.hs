module Main (main) where
  
import Control.Monad (replicateM)
import Control.Monad.Bayes.Class ( MonadDistribution(bernoulli) )
import Control.Monad.Bayes.Sampler.Strict
import Control.Monad.Bayes.Enumerator
import Data.Bits (Bits(xor))

import qualified Data.Set as Set

-- Type definitions
data Field = Field { name :: String, value :: Int }
type Packet = [Field]
type History = [Packet]

-- define show, eq and ord for Field, not really good haskell but makes it a bit more readable at the moment
instance Show Field where
  show (Field name value) = "{" ++ show name ++ ":" ++ show value ++ "}"

-- eq and ord are defined to be able to use Set.Set

instance Eq Field where
  (Field name1 value1) == (Field name2 value2) = name1 == name2 && value1 == value2

instance Ord Field where
  compare (Field name1 value1) (Field name2 value2) = compare name1 name2 <> compare value1 value2


-- not really useful probably but just for testing





pDupp :: History -> History
pDupp [] = []
pDupp (x:xs) = [x,x] ++ xs

printHead :: History -> IO ()
printHead [] = print "Empty history"
printHead (x:xs) = print x

testRegularFunctions = do
    let history = [ [Field "a" 1, Field "b" 2, Field "c" 3]
                  , [Field "a" 4, Field "b" 5, Field "c" 6]
                  , [Field "a" 7, Field "b" 8, Field "c" 9]
                  ]
    printHead []
    printHead history
    let history' = pDupp history
    let history'' = assignHead history' (Field "a" 10)
    printHead history''
    let history''' = assignHead history'' (Field "b" 11)
    printHead history'''
    let history'''' = assignHead history''' (Field "c" 12)
    printHead history''''


-- -- some actual probabilistic stuff

-- -- distribution that has probability p for h1, and 1-p for h2
-- mProbH :: MonadDistribution m => Double -> History -> History -> m History
-- mProbH p h1 h2 = do
--   bernoulli p >>= \b -> return $ if b then h1 else h2

-- -- lift a history to a distribution over that history
-- mHistory :: MonadDistribution m => History -> m History
-- mHistory = return

-- -- mDrop returns the distribution over the empty history
-- mDrop :: MonadDistribution m => m History
-- mDrop = return []

-- -- mDropP maps any distribution to a distribution only containing the empty history
-- mDropP :: MonadDistribution m => m a -> m History
-- mDropP d = do
--   _ <- d
--   return []

-- -- mSkip does not change the distribution
-- mSkip = id

-- -- mITE is a probabilistic if-then-else, for tests
-- mITE :: MonadDistribution m => m Bool -> m a -> m a -> m a
-- mITE b d1 d2 = do
--   x <- b
--   if x then d1 else d2

-- -- test in probnetkat, takes in a history distribution, a field name and a value
-- mTest :: MonadDistribution m => m History -> String -> Int -> m History
-- mTest d name value = do
--   -- if the field 'name' of history 'd' has value 'value', return d, else return the empty history
--   d >>= \h -> if count (Field name value) (head h) > 0 then return h else mDrop


-- --------------



-- mixDist :: MonadDistribution m => Double -> m a -> m a -> m a
-- mixDist p d1 d2 = do
--   x <- bernoulli p
--   if x then d1 else d2

-- -- maps distribution over history to the same distribution but with the head of the history duplicated
-- mDupp :: MonadDistribution m => m History -> m History
-- mDupp d = do
--   h <- d
--   return $ [head h, head h] ++ tail h

-- -- test some functions with IO
-- testBench :: IO ()
-- testBench = do
--   print "Test bench starting"
--   let packet1 = [Field "pt" 1, Field "sw" 2]
--   let history1 = [packet1]

--   let nsamples = 1

--   -- lift the histories to distribtuions
--   let d1 = mHistory history1

--   -- duplicate the head of history1
--   let d1' = mDupp d1

--   -- sample from d1'
--   samples <- sampleIOfixed $ replicateM nsamples d1'

--   -- print samples in d1'
--   print "Samples in d1"
--   print samples

--   print "Test Done"

-- -- more complex testing
-- testBench2 :: IO ()
-- testBench2 = do
--   -- we will create a package (history) and then transform it, and finally sampling it to see the possibilities

--   print "Test bench 2 starting"
--   let packet1 = [Field "pt" 1, Field "sw" 2]

--   let history1 = [packet1]

--   let nsamples = 5

--   -- lift the histories to distribtuions
--   let d1 = mHistory history1

--   -- duplicate the head of history1
--   let d1' = mDupp d1

--   let d1'' = mixDist 0.5 d1' mDrop

--   -- sample from d1'
--   samples <- sampleIOfixed $ replicateM nsamples d1''

--   -- print samples in d1'
--   print "Samples in d1"
--   print samples

--   print "Test bench 2 done"


-- testTest :: IO ()
-- testTest = do
--   let packet1 = [Field "pt" 1, Field "sw" 2]
--   let packet2 = [Field "pt" 2, Field "sw" 2]
--   let d1 = mHistory [packet1]
--   let d2 = mHistory [packet2]

--   let dist = mixDist 0.5 d1 d2

--   let nsamples = 3
  
--   samples <- sampleIOfixed $ replicateM nsamples dist

--   let dist' = mTest dist "pt" 1

--   samples' <- sampleIOfixed $ replicateM nsamples dist'

--   print samples

--   print " "

--   print samples'


-- -- oldTest :: IO ()
-- oldTest = do
--   let f1 = Field "a" 1
--   let f2 = Field "a" 2

--   let p1 = [f1]
--   let p2 = [f2]

--   let h1 = [p1]
--   let h2 = [p2]

--   let nsamples = 1000

--   -- sampleIO vs sampleIOfixed (fixed seed or not)

--   samples' <- sampleIO $ replicateM nsamples (mHistory h1)

--   print $ count h1 samples'

--   samples'' <- sampleIO $ replicateM nsamples (mixDist 0.5 (mHistory h1) (mHistory h2))

--   print $ count h1 samples''
--   print $ count h2 samples''

--   testBench2
  
  -- count the number of 1's in samples
  -- print $ foldl (\acc x -> if x == p1 then acc + 1 else acc) 0 samples

  -- -- count the number of 2's in samples
  -- print $ foldl (\acc x -> if x == p2 then acc + 1 else acc) 0 samples


main :: IO ()
main = do
  print "Hallo"


  {-
  Atomic operations:
  assignment  done.
  test        done?
  dup         done.
  skip        done I think
  drop        done
  p & q       
  p ; q
  p (+)_r q   done

p & q
and
p ; q

are more explicitly programs and not 'just distributions'
how do I deal with that? :l
 
  -}


  {-
  functies moeten van verzamelingen van histories naar distributies over verzamelingen van histories
    -}


----------- Some helper functions
-- count occurences
count :: Eq a => a -> [a] -> Int
count x = length . filter (== x)


-- assign value to a specific field in a packet
assignField :: Packet -> Field -> Packet
assignField [] _ = []
assignField (x:xs) f = if name x == name f then f:xs else x:assignField xs f

-- assign value to the head package of a history
assignHead :: History -> Field -> History
assignHead [] _ = []
assignHead (x:xs) field = assignField x field:xs

dupHead :: History -> History
dupHead [] = []
dupHead (x:xs) = [x,x] ++ xs

----------- Atomic operations
assign :: MonadDistribution m => Field -> Set.Set History -> m (Set.Set History)
assign f hs = do
  let hs' = Set.map (\h -> assignHead h f) hs
  return hs'

test :: MonadDistribution m => Field -> Set.Set History -> m (Set.Set History)
test f hs = do
  let hs' = Set.filter (\h -> count f (head h) > 0) hs
  return hs'

dup :: MonadDistribution m => Set.Set History -> m (Set.Set History)
dup hs = do
  let hs' = Set.map dupHead hs
  return hs'

skip :: MonadDistribution m => Set.Set History -> m (Set.Set History)
skip hs = return hs

drop :: MonadDistribution m => Set.Set History -> m (Set.Set History)
drop hs = return Set.empty


----------- Probabilistic operators
-- p & q

-- p ; q

-- p (+)_r q

-- p & q is parallel composition, so we need to take the union of the two sets of histories
par :: MonadDistribution m => Set.Set History -> Set.Set History -> m (Set.Set History)
par hs1 hs2 = return $ Set.union hs1 hs2

-- p ; q is sequential composition, so we take the distribution of the first program, and then apply the second program to each history in the distribution
seq :: MonadDistribution m => m (Set.Set History) -> (Set.Set History -> m (Set.Set History)) -> m (Set.Set History)
seq hs1 hs2 = do
  hs1' <- hs1
  hs2' <- hs2 hs1'
  return hs2'

-- p (+)_r q is probabilistic with chance r for p and 1-r for q
prob :: MonadDistribution m => Double -> m (Set.Set History) -> m (Set.Set History) -> m (Set.Set History)
prob r hs1 hs2 = do
  x <- bernoulli r
  if x then hs1 else hs2