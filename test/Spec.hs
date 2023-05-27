main :: IO ()
main = hspec $ do
    describe "Probnetkat parser" $ do
        it "can parse assignment" $ do
            parseExp "x <- 1" `shouldBe` EAss (Ident "x") 1
        it "can parse test" $ do
            parseExp "x = 1" `shouldBe` ETest (Ident "x") 1
        it "can parse dup" $ do
            parseExp "dup" `shouldBe` EDup
        it "can parse skip" $ do
            parseExp "skip" `shouldBe` ESkip
        it "can parse drop" $ do
            parseExp "drop" `shouldBe` EDrop
        it "can parse seq" $ do
            parseExp "x <- 1; y <- 2" `shouldBe` ESeq (EAss (Ident "x") 1) (EAss (Ident "y") 2)
        it "can parse prob" $ do
            parseExp "x <- 1 +[0.5] y <- 2" `shouldBe` EProb (EAss (Ident "x") 1) 0.5 (EAss (Ident "y") 2)
        it "can parse par" $ do
            parseExp "x <- 1 & y <- 2" `shouldBe` Epar (EAss (Ident "x") 1) (EAss (Ident "y") 2)