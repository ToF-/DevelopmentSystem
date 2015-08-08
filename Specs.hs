import Test.Hspec
import Model

main = hspec $ do
    describe "a Model" $ do
        it "is a collection of stocks" $ do
            let m = addStock "COAL" 3.7 (addStock "WATER" 5.2 initial)
                s1 = m `stock` "WATER"
                s2 = m `stock` "COAL"

            quantity s1 `shouldBe` 5.2
            quantity s2 `shouldBe` 3.7

        it "can update a stock" $ do
            let m = update "WATER" updater (addStock "WATER" 5.2 initial)
                updater = const 4.5
            quantity (m `stock` "WATER") `shouldBe` 4.5

        it "can increase a stock" $ do
            let m = increase "WATER" increaser (addStock "WATER" 5.2 initial)
                increaser = const 1.5
            quantity (m `stock` "WATER") `shouldBe` 6.7


        it "can decrease a stock" $ do
            let m = decrease "WATER" decreaser (addStock "WATER" 5.0 initial)
                decreaser m = (quantity (m `stock` "WATER")) * 0.1
            quantity (m `stock` "WATER") `shouldBe` 4.5




            
