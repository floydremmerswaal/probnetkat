module Interpreter where

-- import stuff
import Probnetkat.Abs -- abstract syntax tree, is the import correct?

import Probnetkat 

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
    EAss n v -> assign n v
    ETest n v -> test n v
    EDup -> dup
    ESkip -> skip
    EDrop -> drop
    ESeq e1 e2 -> seq (interpret e1) (interpret e2)
    EProb e1 p e2 -> prob p (interpret e1) (interpret e2)
    Epar e1 e2 -> par (interpret e1) ( interpet e2)