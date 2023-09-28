-- File generated by the BNF Converter (bnfc 2.9.4.1).

{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
#if __GLASGOW_HASKELL__ <= 708
{-# LANGUAGE OverlappingInstances #-}
#endif

-- | Pretty-printer for Syntax.

module Syntax.Print where

import Prelude
  ( ($), (.)
  , Bool(..), (==), (<)
  , Int, Integer, Double, (+), (-), (*)
  , String, (++)
  , ShowS, showChar, showString
  , all, elem, foldr, id, map, null, replicate, shows, span
  )
import Data.Char ( Char, isSpace )
import qualified Syntax.Abs

-- | The top-level printing method.

printTree :: Print a => a -> String
printTree = render . prt 0

type Doc = [ShowS] -> [ShowS]

doc :: ShowS -> Doc
doc = (:)

render :: Doc -> String
render d = rend 0 False (map ($ "") $ d []) ""
  where
  rend
    :: Int        -- ^ Indentation level.
    -> Bool       -- ^ Pending indentation to be output before next character?
    -> [String]
    -> ShowS
  rend i p = \case
      "["      :ts -> char '[' . rend i False ts
      "("      :ts -> char '(' . rend i False ts
      "{"      :ts -> onNewLine i     p . showChar   '{'  . new (i+1) ts
      "}" : ";":ts -> onNewLine (i-1) p . showString "};" . new (i-1) ts
      "}"      :ts -> onNewLine (i-1) p . showChar   '}'  . new (i-1) ts
      [";"]        -> char ';'
      ";"      :ts -> char ';' . new i ts
      t  : ts@(s:_) | closingOrPunctuation s
                   -> pending . showString t . rend i False ts
      t        :ts -> pending . space t      . rend i False ts
      []           -> id
    where
    -- Output character after pending indentation.
    char :: Char -> ShowS
    char c = pending . showChar c

    -- Output pending indentation.
    pending :: ShowS
    pending = if p then indent i else id

  -- Indentation (spaces) for given indentation level.
  indent :: Int -> ShowS
  indent i = replicateS (2*i) (showChar ' ')

  -- Continue rendering in new line with new indentation.
  new :: Int -> [String] -> ShowS
  new j ts = showChar '\n' . rend j True ts

  -- Make sure we are on a fresh line.
  onNewLine :: Int -> Bool -> ShowS
  onNewLine i p = (if p then id else showChar '\n') . indent i

  -- Separate given string from following text by a space (if needed).
  space :: String -> ShowS
  space t s =
    case (all isSpace t', null spc, null rest) of
      (True , _   , True ) -> []              -- remove trailing space
      (False, _   , True ) -> t'              -- remove trailing space
      (False, True, False) -> t' ++ ' ' : s   -- add space if none
      _                    -> t' ++ s
    where
      t'          = showString t []
      (spc, rest) = span isSpace s

  closingOrPunctuation :: String -> Bool
  closingOrPunctuation [c] = c `elem` closerOrPunct
  closingOrPunctuation _   = False

  closerOrPunct :: String
  closerOrPunct = ")],;"

parenth :: Doc -> Doc
parenth ss = doc (showChar '(') . ss . doc (showChar ')')

concatS :: [ShowS] -> ShowS
concatS = foldr (.) id

concatD :: [Doc] -> Doc
concatD = foldr (.) id

replicateS :: Int -> ShowS -> ShowS
replicateS n f = concatS (replicate n f)

-- | The printer class does the job.

class Print a where
  prt :: Int -> a -> Doc

instance {-# OVERLAPPABLE #-} Print a => Print [a] where
  prt i = concatD . map (prt i)

instance Print Char where
  prt _ c = doc (showChar '\'' . mkEsc '\'' c . showChar '\'')

instance Print String where
  prt _ = printString

printString :: String -> Doc
printString s = doc (showChar '"' . concatS (map (mkEsc '"') s) . showChar '"')

mkEsc :: Char -> Char -> ShowS
mkEsc q = \case
  s | s == q -> showChar '\\' . showChar s
  '\\' -> showString "\\\\"
  '\n' -> showString "\\n"
  '\t' -> showString "\\t"
  s -> showChar s

prPrec :: Int -> Int -> Doc -> Doc
prPrec i j = if j < i then parenth else id

instance Print Integer where
  prt _ x = doc (shows x)

instance Print Double where
  prt _ x = doc (shows x)

instance Print Syntax.Abs.Exp where
  prt i = \case
    Syntax.Abs.EAssSw n -> prPrec i 3 (concatD [doc (showString "sw"), doc (showString "<-"), prt 0 n])
    Syntax.Abs.EAssPt n -> prPrec i 3 (concatD [doc (showString "pt"), doc (showString "<-"), prt 0 n])
    Syntax.Abs.ESwEq n -> prPrec i 3 (concatD [doc (showString "sw"), doc (showString "="), prt 0 n])
    Syntax.Abs.EPtEq n -> prPrec i 3 (concatD [doc (showString "pt"), doc (showString "="), prt 0 n])
    Syntax.Abs.ESwNEq n -> prPrec i 3 (concatD [doc (showString "sw"), doc (showString "!="), prt 0 n])
    Syntax.Abs.EPtNEq n -> prPrec i 3 (concatD [doc (showString "pt"), doc (showString "!="), prt 0 n])
    Syntax.Abs.EDup -> prPrec i 3 (concatD [doc (showString "dup")])
    Syntax.Abs.ESkip -> prPrec i 3 (concatD [doc (showString "skip")])
    Syntax.Abs.EDrop -> prPrec i 3 (concatD [doc (showString "drop")])
    Syntax.Abs.ESeq exp1 exp2 -> prPrec i 2 (concatD [prt 2 exp1, doc (showString ";"), prt 3 exp2])
    Syntax.Abs.EProbD exp1 exp2 -> prPrec i 1 (concatD [prt 1 exp1, doc (showString "+"), prt 2 exp2])
    Syntax.Abs.EProb exp1 d exp2 -> prPrec i 1 (concatD [prt 1 exp1, doc (showString "+["), prt 0 d, doc (showString "]"), prt 2 exp2])
    Syntax.Abs.EPar exp1 exp2 -> prPrec i 0 (concatD [prt 0 exp1, doc (showString "&"), prt 1 exp2])
    Syntax.Abs.EKleene exp -> prPrec i 0 (concatD [prt 0 exp, doc (showString "*")])
