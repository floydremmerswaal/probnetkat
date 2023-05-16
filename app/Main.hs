module Main (main) where
  
import Control.Monad (replicateM)
import Control.Monad.Bayes.Class ( MonadDistribution(bernoulli) )
import Control.Monad.Bayes.Sampler.Strict
import Control.Monad.Bayes.Enumerator
import Data.Bits (Bits(xor))

import qualified Data.Set as Set
import Data.Set (Set)

import Data.Maybe (listToMaybe)

-- Type definitions
data Field = Field { name :: String, value :: Int } deriving (Eq, Ord)
type Packet = [Field]
type History = [Packet]

type SH = Set History

-- define show not really good haskell but makes it a bit more readable at the moment
instance Show Field where
  show (Field name value) = "{" ++ show name ++ ":" ++ show value ++ "}"

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
assign :: MonadDistribution m => Field -> SH -> m (SH)
assign f hs = do
  let hs' = Set.map (\h -> assignHead h f) hs
  return hs'

test :: MonadDistribution m => Field -> SH -> m (SH)
test f = return . Set.filter (any (any (== f)) . listToMaybe)

dup :: MonadDistribution m => SH -> m (SH)
dup hs = return . Set.map dupHead

skip :: MonadDistribution m => SH -> m (SH)
skip hs = return hs

drop :: MonadDistribution m => SH -> m (SH)
drop hs = return Set.empty


----------- Probabilistic operators
-- p & q
-- p ; q
-- p (+)_r q


-- p & q is parallel composition, so we need to take the union of the two sets of histories
parSets :: MonadDistribution m => SH -> SH -> m (SH)
parSets hs1 hs2 = return $ Set.union hs1 hs2

-- par on programs
-- par :: MonadDistribution m => (SH -> m (SH)) -> (SH -> m (SH)) -> (SH -> m (SH))
-- par prgm1 prgm2 = do
--   let o1 = prgm1
--   let o2 = prgm2
--   return $ parSets o1 o2

-- p ; q is sequential composition
seq :: MonadDistribution m => (SH -> m (SH)) -> (SH -> m (SH)) -> (SH -> m (SH))
seq prgm1 prgm2 h = do
  prgm1 h >>= prgm2

-- p (+)_r q is probabilistic with chance r for p and 1-r for q
prob :: MonadDistribution m => Double -> m (SH) -> m (SH) -> m (SH)
prob r hs1 hs2 = do
  x <- bernoulli r
  if x then hs1 else hs2