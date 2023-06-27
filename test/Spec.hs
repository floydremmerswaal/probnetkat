import Syntax.Abs
import Test.Hspec
import Transformation
import Semantics
import Syntax.Par (myLexer, pExp)
import Control.Monad.Bayes.Enumerator
import Control.Arrow
import qualified Data.Set as Set


stringToExp :: String -> Exp
stringToExp s = case pExp (myLexer s) of
    Left _ -> ESkip
    Right tree -> tree


-- not used/working yet
testTransExp :: Syntax.Abs.Exp -> [(SH, Double)]
testTransExp tree = do
    let history = [[]] :: History
    let initialSet = Set.fromList [history] :: SH
    let kleisliArrow = transExp tree :: Kleisli Enumerator SH SH
    let result = runKleisli kleisliArrow initialSet
    enumerator result

main :: IO ()
main = hspec $ do
    describe "Probnetkat parser" $ do
        it "can parse assignment" $ do
            stringToExp "x <- 1" `shouldBe` EAss (Ident "x") 1
        it "can parse test" $ do
            stringToExp "x = 1" `shouldBe` EEq (Ident "x") 1
        it "can parse negative test" $ do
            stringToExp "x != 1" `shouldBe` ENeq (Ident "x") 1
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