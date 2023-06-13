-- File generated by the BNF Converter (bnfc 2.9.4.1).

-- Templates for pattern matching on abstract syntax

{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Transformation where

import Prelude hiding (drop, seq)
import qualified Syntax.Abs

import Control.Monad.Bayes.Class

import Control.Arrow
import Semantics 

transExp :: MonadDistribution m => Syntax.Abs.Exp -> Kleisli m SH SH
transExp x = case x of
  Syntax.Abs.EAss ident integer -> assign (Field ident integer)
  Syntax.Abs.ETest ident integer -> test (Field ident integer)
  Syntax.Abs.EDup -> dup
  Syntax.Abs.ESkip -> skip
  Syntax.Abs.EDrop -> drop
  Syntax.Abs.ESeq exp1 exp2 -> seq (transExp exp1) (transExp exp2)
  Syntax.Abs.EProb exp1 double exp2 -> prob double (transExp exp1)  (transExp exp2)
  Syntax.Abs.Epar exp1 exp2 -> par (transExp exp1) (transExp exp2)
