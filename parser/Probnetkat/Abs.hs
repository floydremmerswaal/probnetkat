-- File generated by the BNF Converter (bnfc 2.9.4.1).

{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- | The abstract syntax of language probnetkat.

module Probnetkat.Abs where

import Prelude (Double, Integer, String)
import qualified Prelude as C (Eq, Ord, Show, Read)
import qualified Data.String

data Exp
    = EAss Ident Integer
    | ETest Ident Integer
    | EDup
    | ESkip
    | EDrop
    | ESeq Exp Exp
    | EProb Exp Double Exp
    | Epar Exp Exp
  deriving (C.Eq, C.Ord, C.Show, C.Read)

newtype Ident = Ident String
  deriving (C.Eq, C.Ord, C.Show, C.Read, Data.String.IsString)

