import Test.Hspec
import Model

main = hspec $ do
    describe "Stock" $ do
        describe "has a quantity that" $ do
            it "is initially zero" $ do
                quantity initial `shouldBe` 0.0

            it "can be increased by input" $ do
                quantity (input initial 1.4) `shouldBe` 1.4
                
                quantity (input (input initial 1.4) 3.2) `shouldBe` 4.6

            it "can be decreased by output" $ do
                let m = (input initial 4.0)
                quantity (output m 1.0) `shouldBe` 3.0

            it "cannot be negative" $ do
                let m = input initial 1.0
                quantity (output m 1.4) `shouldBe` 0.0

        describe "has an input flow that" $ do
            it "is initially zero" $ do
                input_flow initial `shouldBe` 0

            it "is set via input" $ do
                input_flow (input initial 3.7) `shouldBe` 3.7
        
        describe "has an output flow that" $ do
            it "is initially zero" $ do
                output_flow initial `shouldBe` 0

            it "is set via output" $ do
                let m = output (input initial 4.7) 1.0
                output_flow m `shouldBe` 1.0

            it "cannot be greater than stock" $ do
                let m = output (input initial 0.6) 1.0
                output_flow m `shouldBe` 0.6

    describe "a throughput" $ do
        describe "does output to a stock" $ do
            it "according to a function of the stock" $ do
                let s = input initial 4.0
                    tenpercent stock = (quantity stock) * 0.25
                    s'= throughput s tenpercent
                output_flow s' `shouldBe` 1.0

                let s = input initial 4.0
                    s'= throughput s (const 1.0)
                quantity s' `shouldBe` 3.0
                
                
                
                
    

                

            

                
                



