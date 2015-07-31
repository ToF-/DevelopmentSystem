import Test.Hspec
import Model

main = hspec $ do
    describe "Stock" $ do
        describe "has a quantity that" $ do
            it "is initially zero" $ do
                let m = initial
                quantity m `shouldBe` 0.0

            it "can be increased by input" $ do
                let m = input initial 1.4
                quantity m `shouldBe` 1.4
                
                let m = input (input initial 1.4) 3.2
                quantity m `shouldBe` 4.6

            it "can be decreased by output" $ do
                let m = input initial 4.0
                    m'= output m (1.0) 
                quantity m' `shouldBe` 3.0

            it "cannot be negative" $ do
                let m = input initial 1.0
                    m' = output m (1.4)
                quantity m' `shouldBe` 0.0

        describe "has an input flow that" $ do
            it "is initially zero" $ do
                input_flow initial `shouldBe` 0

            it "is set via input" $ do
                let m = input initial 3.7
                input_flow m `shouldBe` 3.7
        
        describe "has an output flow that" $ do
            it "is initially zero" $ do
                output_flow initial `shouldBe` 0

            it "is set via output" $ do
                let m = output (input initial 4.7) 1.0
                output_flow m `shouldBe` 1.0

            it "cannot be greater than stock" $ do
                let m = output (input initial 0.6) 1.0
                output_flow m `shouldBe` 0.6

                

            

                
                



