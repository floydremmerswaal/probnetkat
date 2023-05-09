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

-- eq and ord are defined to be able to use Set.Set (uses a binary tree under the hood, relying on ord)

instance Eq Field where
  (Field name1 value1) == (Field name2 value2) = name1 == name2 && value1 == value2

instance Ord Field where
  compare (Field name1 value1) (Field name2 value2) = compare name1 name2 <> compare value1 value2

main :: IO ()
main = do
  print "Hallo"

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
parSets :: MonadDistribution m => Set.Set History -> Set.Set History -> m (Set.Set History)
parSets hs1 hs2 = return $ Set.union hs1 hs2

-- par on programs
-- par :: MonadDistribution m => (Set.Set History -> m (Set.Set History)) -> (Set.Set History -> m (Set.Set History)) -> (Set.Set History -> m (Set.Set History))
-- par prgm1 prgm2 = do
--   let o1 = prgm1
--   let o2 = prgm2
--   return $ parSets o1 o2

-- p ; q is sequential composition
seq :: MonadDistribution m => (Set.Set History -> m (Set.Set History)) -> (Set.Set History -> m (Set.Set History)) -> (Set.Set History -> m (Set.Set History))
seq prgm1 prgm2 = do
  prgm1 >>= prgm2

-- p (+)_r q is probabilistic with chance r for p and 1-r for q
prob :: MonadDistribution m => Double -> m (Set.Set History) -> m (Set.Set History) -> m (Set.Set History)
prob r hs1 hs2 = do
  x <- bernoulli r
  if x then hs1 else hs2