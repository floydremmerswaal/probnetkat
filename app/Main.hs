module Main (main, assign, test, dup, skip, drop, seq, prob, par) where

import Prelude hiding (id, (.), drop, seq)

import Control.Monad.Bayes.Class ( MonadDistribution(bernoulli) )
import Control.Arrow
import Control.Category

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
  show (Field n v) = "{" ++ show n ++ ":" ++ show v ++ "}"

main :: IO ()
main = do
  print "Hallo"

----------- Some helper functions

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
assign :: MonadDistribution m => Field -> Kleisli m SH SH
assign f = arr $ Set.map (`assignHead` f)

test :: MonadDistribution m => Field -> Kleisli m SH SH
test f = arr $ Set.filter (any (elem f) . listToMaybe)

dup :: MonadDistribution m => Kleisli m SH SH
dup = arr $ Set.map dupHead

skip :: MonadDistribution m => Kleisli m SH SH
skip = id

drop :: MonadDistribution m => Kleisli m SH SH
drop = arr $ const Set.empty


----------- Probabilistic operators
-- p & q
-- p ; q
-- p (+)_r q


-- p & q is parallel composition, so we need to take the union of the two sets of histories
parOld :: MonadDistribution m => SH -> (SH -> m SH) -> (SH -> m (SH)) -> m (SH)
parOld sh prgm1 prgm2 = do
  sample1 <- prgm1 sh
  sample2 <- prgm2 sh
  return $ Set.union sample1 sample2
-- Dit kan elegeant met Arrow? LiftA2?

par :: MonadDistribution m => Kleisli m SH SH -> Kleisli m SH SH -> Kleisli m SH SH
par = liftA2 Set.union

-- par on programs
-- par :: MonadDistribution m => (SH -> m (SH)) -> (SH -> m (SH)) -> (SH -> m (SH))
-- par prgm1 prgm2 = do
--   let o1 = prgm1
--   let o2 = prgm2
--   return $ parSets o1 o2

-- p ; q is sequential composition
seq :: MonadDistribution m => Kleisli m SH SH  -> Kleisli m SH SH  -> Kleisli m SH SH
seq = (>>>)


-- old seq, difference in type signature!
-- seq :: MonadDistribution m => (SH -> m (SH)) -> (SH -> m (SH)) -> (SH -> m (SH))
-- seq prgm1 prgm2 h = do
--   prgm1 h >>= prgm2

-- p (+)_r q is probabilistic with chance r for p and 1-r for q
prob :: MonadDistribution m => Double -> m SH -> m SH -> m SH
prob r hs1 hs2 = do
  x <- bernoulli r
  if x then hs1 else hs2

