-- File generated by the BNF Converter (bnfc 2.9.4.1).

-- Templates for pattern matching on abstract syntax

{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module Probnetkat.Skel where

import Prelude (($), Either(..), String, (++), Show, show)
import qualified Probnetkat.Abs

type Err = Either String
type Result = Err String

failure :: Show a => a -> Result
failure x = Left $ "Undefined case: " ++ show x

transIdent :: Probnetkat.Abs.Ident -> Result
transIdent x = case x of
  Probnetkat.Abs.Ident string -> failure x

transExp :: Probnetkat.Abs.Exp -> Result
transExp x = case x of
  Probnetkat.Abs.EAss ident integer -> failure x
  Probnetkat.Abs.ETest ident integer -> failure x
  Probnetkat.Abs.EDup -> failure x
  Probnetkat.Abs.ESkip -> failure x
  Probnetkat.Abs.EDrop -> failure x
  Probnetkat.Abs.ESeq exp1 exp2 -> failure x
  Probnetkat.Abs.EProb exp1 double exp2 -> failure x
  Probnetkat.Abs.Epar exp1 exp2 -> failure x