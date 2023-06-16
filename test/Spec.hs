import Syntax.Abs
import Test.Hspec
import Transformation
import Semantics
import Syntax.Par (myLexer, pExp)
import Control.Monad.Bayes.Enumerator
import Control.Arrow

stringToExp :: String -> Exp
stringToExp s = case pExp (myLexer s) of
    Left _ -> ESkip
    Right tree -> tree

testTransExp :: Syntax.Abs.Exp -> Kleisli Enumerator SH SH
testTransExp = transExp

main :: IO ()
main = hspec $ do
    describe "Probnetkat parser" $ do
        it "can parse assignment" $ do
            stringToExp "x <- 1" `shouldBe` EAss (Ident "x") 1
        it "can parse test" $ do
            stringToExp "x = 1" `shouldBe` ETest (Ident "x") 1
        it "can parse dup" $ do
            stringToExp "dup" `shouldBe` EDup
        it "can parse skip" $ do
            stringToExp "skip" `shouldBe` ESkip
        it "can parse drop" $ do
            stringToExp "drop" `shouldBe` EDrop
        it "can parse seq" $ do
            stringToExp "x <- 1; y <- 2" `shouldBe` ESeq (EAss (Ident "x") 1) (EAss (Ident "y") 2)
        it "can parse prob" $ do
            stringToExp "x <- 1 +[0.5] y <- 2" `shouldBe` EProb (EAss (Ident "x") 1) 0.5 (EAss (Ident "y") 2)
        it "can parse par" $ do
            stringToExp "x <- 1 & y <- 2" `shouldBe` Epar (EAss (Ident "x") 1) (EAss (Ident "y") 2)
    -- TODO: testing the transformation    
    -- describe "Probnetkat transformation" $ do
    --         it "can transform assignment" $ do
    --             testTransExp (stringToExp "x <- 1") `shouldBe` assign (Field (Ident "x") 1)