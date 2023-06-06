module Semantics (main, assign, test, dup, skip, drop, seq, prob, par, Field(..), Packet, History, SH) where

import Prelude hiding (id, (.), drop, seq)

import Control.Monad.Bayes.Class ( MonadDistribution(bernoulli) )
import Control.Arrow
import Control.Category
import Control.Applicative (liftA2)

import qualified Data.Set as Set
import Data.Set (Set)

import Data.Maybe (listToMaybe)

-- Type definitions
data Field = Field { name :: String, value :: Int } deriving (Eq, Ord)
type Packet = [Field]
type History = [Packet]

type SH = Set History

type KSH m = Kleisli m SH SH

-- define show, not really good haskell but makes it a bit more readable at the moment
instance Show Field where
  show (Field n v) = "{" ++ show n ++ ":" ++ show v ++ "}"

-- main function
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

--- assign uses a Field at the moment, we might want to use string and int instead in the future
assign :: MonadDistribution m => Field -> KSH m
assign f = arr $ Set.map (`assignHead` f)

test :: MonadDistribution m => Field -> KSH m
test f = arr $ Set.filter (any (elem f) . listToMaybe)

dup :: MonadDistribution m => KSH m
dup = arr $ Set.map dupHead

skip :: MonadDistribution m => KSH m
skip = id

drop :: MonadDistribution m => KSH m
drop = arr $ const Set.empty


----------- Probabilistic operators
-- p & q
-- p ; q
-- p (+)_r q


-- p & q is parallel composition, so we need to take the union of the two sets of histories
-- parOld :: MonadDistribution m => SH -> (SH -> m SH) -> (SH -> m (SH)) -> m (SH)
-- parOld sh prgm1 prgm2 = do
--   sample1 <- prgm1 sh
--   sample2 <- prgm2 sh
--   return $ Set.union sample1 sample2
-- Dit kan elegeant met Arrow? LiftA2?

par :: MonadDistribution m => KSH m -> KSH m -> KSH m
par = liftA2 Set.union

-- par on programs
-- par :: MonadDistribution m => (SH -> m (SH)) -> (SH -> m (SH)) -> (SH -> m (SH))
-- par prgm1 prgm2 = do
--   let o1 = prgm1
--   let o2 = prgm2
--   return $ parSets o1 o2

-- p ; q is sequential composition
seq :: MonadDistribution m => KSH m  -> KSH m  -> KSH m
seq = (>>>)


-- old seq, difference in type signature!
-- seq :: MonadDistribution m => (SH -> m (SH)) -> (SH -> m (SH)) -> (SH -> m (SH))
-- seq prgm1 prgm2 h = do
--   prgm1 h >>= prgm2

-- p (+)_r q is probabilistic with chance r for p and 1-r for q
-- probOld :: MonadDistribution m => Double -> m SH -> m SH -> m SH
-- probOld r hs1 hs2 = do
--   x <- bernoulli r
--   if x then hs1 else hs2


-- probK :: MonadDistribution m => Double -> Kleisli m SH SH -> Kleisli m SH SH -> Kleisli m SH SH
-- prob r f g = Kleisli $ \h -> 
-- Henning zegt:do 
-- Henning zegt:x <- bernoulli r 
-- Henning zegt:if x then f h else g h 

-- f h moet nog runKleisli f h oid zijn

prob :: MonadDistribution m => Double -> KSH m -> KSH m -> KSH m
prob r f g = Kleisli $ \h -> do
  x <- bernoulli r
  if x then runKleisli f h else runKleisli g h