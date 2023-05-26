module Interpreter where

-- import stuff
import Probnetkat.Abs -- abstract syntax tree, is the import correct?

import app.Main 

-- 
-- data Exp
--     = EAss Ident Integer
--     | ETest Ident Integer
--     | EDup
--     | ESkip
--     | EDrop
--     | ESeq Exp Exp
--     | EProb Exp Double Exp
--     | Epar Exp Exp
--   deriving (C.Eq, C.Ord, C.Show, C.Read)
--

interpret :: Exp -> Integer
interpret x = case x of
    EAss = assign
    ETest = test
    EDup = dup
    ESkip = skip
    EDrop = drop
    ESeq = seq
    EProb = prob
    Epar = par