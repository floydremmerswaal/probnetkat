module Semantics (assign, test, testneq, dup, skip, drop, seq, prob, par, Field(..), Packet, History, SH, KSH) where

import Prelude hiding (id, (.), drop, seq)

import Control.Monad.Bayes.Class ( MonadDistribution(bernoulli) )
import Control.Arrow
import Control.Category
import Control.Applicative (liftA2)

import qualified Data.Set as Set
import Data.Set (Set)

import Data.Maybe (listToMaybe)

import Syntax.Abs (Ident(..))

-- Type definitions
data Field = Field { name :: Ident, value :: Integer } deriving (Eq, Ord)
type Packet = [Field]
type History = [Packet]

type SH = Set History

type KSH m = Kleisli m SH SH

-- define show, not really good haskell but makes it a bit more readable at the moment
instance Show Field where
  show (Field n v) = "{" ++ show n ++ ":" ++ show v ++ "}"


----------- Some helper functions

-- assign value to a specific field in a packet
assignField :: Packet -> Field -> Packet
assignField [] _ = []
assignField (x:xs) f = if name x == name f then f:xs else x:assignField xs f

-- assignField, but if the field is not present, add it
assignField' :: Packet -> Field -> Packet
assignField' [] f = [f]
assignField' (x:xs) f = if name x == name f then f:xs else x:assignField' xs f

-- assign value to the head package of a history
assignHead :: History -> Field -> History
assignHead [] _ = []
assignHead (x:xs) field = assignField x field:xs

-- assignHead but if the field is not present, add it
assignHead' :: History -> Field -> History
assignHead' [] f = [[f]]
assignHead' (x:xs) f = assignField' x f:xs

dupHead :: History -> History
dupHead [] = []
dupHead (x:xs) = [x,x] ++ xs

-- assign but if the input is the empty set, apply assign to the empty set
-- assign :: MonadDistribution m => Field -> KSH m
-- assign f = Kleisli $ \h -> if Set.null h then return $ Set.fromList [assignHead' [] f] else runKleisli (assign' f) h


assign :: MonadDistribution m => Field -> KSH m
assign f = arr $ Set.map (`assignHead'` f)

-- negation of test
testneq' :: MonadDistribution m => Field -> KSH m
testneq' f = arr $ Set.filter (all (notElem f))

testneq :: MonadDistribution m => Field -> KSH m
testneq f = testneq' f >>> fixEmpty

-- filter out all histories that do not contain the given field (with value)
test'  :: MonadDistribution m => Field -> KSH m
test' f = arr $ Set.filter (any (elem f) . listToMaybe)

test :: MonadDistribution m => Field -> Kleisli m SH SH
test f = test' f >>> fixEmpty

-- map the empty set to the set containing an empty history, otherwise return the set itself
fixEmpty :: MonadDistribution m => KSH m
fixEmpty = arr $ \h -> if Set.null h then Set.singleton [] else h

dup :: MonadDistribution m => KSH m
dup = arr $ Set.map dupHead

skip :: MonadDistribution m => KSH m
skip = id

drop :: MonadDistribution m => KSH m
drop = arr $ const (Set.singleton [])


----------- Other operators

-- p & q is parallel composition
par :: MonadDistribution m => KSH m -> KSH m -> KSH m
par = liftA2 Set.union

-- p ; q is sequential composition
seq :: MonadDistribution m => KSH m  -> KSH m  -> KSH m
seq = (>>>)

-- p (+)_r q is probabilistic choice
prob :: MonadDistribution m => Double -> KSH m -> KSH m -> KSH m
prob r f g = Kleisli $ \h -> do
  x <- bernoulli r
  if x then runKleisli f h else runKleisli g h