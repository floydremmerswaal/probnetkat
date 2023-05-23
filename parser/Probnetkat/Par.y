-- -*- haskell -*- File generated by the BNF Converter (bnfc 2.9.4.1).

-- Parser definition for use with Happy
{
{-# OPTIONS_GHC -fno-warn-incomplete-patterns -fno-warn-overlapping-patterns #-}
{-# LANGUAGE PatternSynonyms #-}

module Probnetkat.Par
  ( happyError
  , myLexer
  , pExp
  ) where

import Prelude

import qualified Probnetkat.Abs
import Probnetkat.Lex

}

%name pExp Exp
-- no lexer declaration
%monad { Err } { (>>=) } { return }
%tokentype {Token}
%token
  '&'      { PT _ (TS _ 1)  }
  '('      { PT _ (TS _ 2)  }
  ')'      { PT _ (TS _ 3)  }
  '+['     { PT _ (TS _ 4)  }
  ';'      { PT _ (TS _ 5)  }
  '<-'     { PT _ (TS _ 6)  }
  '='      { PT _ (TS _ 7)  }
  ']'      { PT _ (TS _ 8)  }
  'drop'   { PT _ (TS _ 9)  }
  'dup'    { PT _ (TS _ 10) }
  'skip'   { PT _ (TS _ 11) }
  L_Ident  { PT _ (TV $$)   }
  L_doubl  { PT _ (TD $$)   }
  L_integ  { PT _ (TI $$)   }

%%

Ident :: { Probnetkat.Abs.Ident }
Ident  : L_Ident { Probnetkat.Abs.Ident $1 }

Double  :: { Double }
Double   : L_doubl  { (read $1) :: Double }

Integer :: { Integer }
Integer  : L_integ  { (read $1) :: Integer }

Exp3 :: { Probnetkat.Abs.Exp }
Exp3
  : Ident '<-' Integer { Probnetkat.Abs.EAss $1 $3 }
  | Ident '=' Integer { Probnetkat.Abs.ETest $1 $3 }
  | 'dup' { Probnetkat.Abs.EDup }
  | 'skip' { Probnetkat.Abs.ESkip }
  | 'drop' { Probnetkat.Abs.EDrop }
  | '(' Exp ')' { $2 }

Exp2 :: { Probnetkat.Abs.Exp }
Exp2 : Exp2 ';' Exp3 { Probnetkat.Abs.ESeq $1 $3 } | Exp3 { $1 }

Exp1 :: { Probnetkat.Abs.Exp }
Exp1
  : Exp1 '+[' Double ']' Exp2 { Probnetkat.Abs.EProb $1 $3 $5 }
  | Exp2 { $1 }

Exp :: { Probnetkat.Abs.Exp }
Exp : Exp '&' Exp1 { Probnetkat.Abs.Epar $1 $3 } | Exp1 { $1 }

{

type Err = Either String

happyError :: [Token] -> Err a
happyError ts = Left $
  "syntax error at " ++ tokenPos ts ++
  case ts of
    []      -> []
    [Err _] -> " due to lexer error"
    t:_     -> " before `" ++ (prToken t) ++ "'"

myLexer :: String -> [Token]
myLexer = tokens

}

