import Test.Hspec
import Model

main = hspec $ do
    describe "a Stock" $ do
        let w = Stock "WATER" 5.2  1.0  0.8

        it "has a label" $ do
            label w `shouldBe` "WATER"

        it "has a quantity" $ do
            quantity w `shouldBe` 5.2

        it "has an input" $ do
            input w `shouldBe` 1.0

        it "has an output" $ do
            output w `shouldBe` 0.8

    describe "a Model" $ do
        let m = addStock c (addStock w initial)
            w = Stock "WATER" 5.2 1.0 0.8
            c = Stock "COAL"  4.9 1.0 1.0

        it "is a collection of Stocks" $ do
            length m `shouldBe` 2

        it "can find its stocks by name" $ do
            stock m "WATER" `shouldBe` w

        it "can update its stocks" $ do
            let w' = w { quantity = 3.6 }
                m' = update "WATER" w' m
            stock m' "WATER" `shouldBe` w'
            stock m' "COAL"  `shouldBe` c

    describe "an runOutput function" $ do
        let w = Stock "WATER" 5.0 1.0 0.8

        it "updates a stock quantity and output values" $ do
            let w' = runOutput f w
                f s = quantity s * 0.1
            quantity w' `shouldBe` 4.5    
            output w'   `shouldBe` 0.5

        it "can update a stock output with othe parameters" $ do    
            let w' = runOutput (f 0.2) w
                f limit s = min limit (quantity s * 0.1)
            quantity w' `shouldBe` 4.8
            output w'   `shouldBe` 0.2
            



        
            
            
    

        
        



