import Test.Hspec
import Model

main = hspec $ do
    describe "a very simple development system" $ do
        it "should output its stock limited by capacity" $ do
            let m = addLimit ("capacity",1.0) initial
                m' = output (input 0.0 m)
            inputValue m' `shouldBe` 0.0 
            outputValue m' `shouldBe` 0.0
            quantity m' `shouldBe` 0.0
            
