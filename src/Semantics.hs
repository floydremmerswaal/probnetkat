module Semantics (assignSw, assignPt, testSw, testPt, dup, skip, drop, seq, prob, par, kleene, Packet, History, SH, KSH, pt, sw) where

import Prelude hiding (id, (.), drop, seq)

import Control.Monad.Bayes.Class ( MonadDistribution(bernoulli) )
import Control.Arrow
import Control.Category ( Category(id) )
import Control.Applicative (liftA2)

import Data.Function (fix)

import qualified Data.Set as Set
import Data.Set (Set)

-- import Data.Maybe (listToMaybe)

-- import Syntax.Abs (Ident(..))

-- Type definitions
type Packet = (Integer, Integer) -- sw is the first element, pt is the second
type History = [Packet]

type SH = Set History

type KSH m = Kleisli m SH SH
----------- Some helper functions

pt :: Packet -> Integer
pt (_,y) = y

sw :: Packet -> Integer
sw (x,_) = x

dupHead :: History -> History
dupHead [] = []
dupHead (x:xs) = [x,x] ++ xs

changeSw :: Integer -> History -> History
changeSw _ [] = []
changeSw i ((_,y):xs) = (i,y) : xs

changePt :: Integer -> History -> History
changePt _ [] = []
changePt i ((x,_):xs) = (x,i) : xs

assignSw :: MonadDistribution m => Integer -> KSH m
assignSw s = arr $ Set.map (changeSw s)

assignPt :: MonadDistribution m => Integer -> KSH m
assignPt t = arr $ Set.map (changePt t)

testSwPacket :: Bool -> Integer -> History -> Bool
testSwPacket True s (x:_) = sw x == s 
testSwPacket False s (x:_) = sw x /= s
testSwPacket _ _ [] = False

testPtPacket :: Bool -> Integer -> History -> Bool
testPtPacket True p (x:_) = pt x == p
testPtPacket False p (x:_) = pt x /= p
testPtPacket _ _ [] = False

testSw' :: MonadDistribution m => Bool -> Integer -> KSH m
testSw' b s = arr $ Set.filter (testSwPacket b s)

testPt' :: MonadDistribution m => Bool -> Integer -> KSH m
testPt' b t = arr $ Set.filter (testPtPacket b t)

-- tests

testSw :: MonadDistribution m => Integer -> Kleisli m SH SH
testSw v = fixEmpty >>> testSw' True v

testPt :: MonadDistribution m => Integer -> Kleisli m SH SH
testPt v = fixEmpty >>> testPt' True v

-- testNegSw :: MonadDistribution m => Integer -> Kleisli m SH SH
-- testNegSw v = fixEmpty >>> testSw' False v

-- testNegPt :: MonadDistribution m => Integer -> Kleisli m SH SH
-- testNegPt v = fixEmpty >>> testSw' False v

-- map the empty set to the set containing an empty history, otherwise return the set itself
fixEmpty :: MonadDistribution m => KSH m
fixEmpty = arr $ \h -> if Set.null h then Set.singleton [] else h

dup :: MonadDistribution m => KSH m
dup = arr $ Set.map dupHead

skip :: MonadDistribution m => KSH m
skip = id

drop :: MonadDistribution m => KSH m
drop = arr $ const (Set.singleton [])


-- approximate Kleene star by a finite number of iterations
-- the paper specifies approximating by doing (skip & p)^n
kleeneApprox :: MonadDistribution m => Integer -> KSH m -> KSH m
kleeneApprox 0 _ = skip
kleeneApprox n p = skip `par` seq p (kleeneApprox (n-1) p)

kleene :: MonadDistribution m => KSH m -> KSH m
kleene = kleeneApprox 5

-- do kleeneApprox repetitions until we reach a fixpoint


kleeneRec :: MonadDistribution m => KSH m -> KSH m
kleeneRec p = skip `par` seq p (kleeneRec p)

-- kleeneRecFix :: MonadDistribution m => KSH m -> KSH m
-- kleeneRecFix = loop kleeneRec

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
