import Test.Hspec
import Model

main = hspec $ do
    describe "a very simple development system" $ do
        it "should output its stock limited by capacity" $ do
            let m   = setTeamCapacity initial 1.0
                m'  = input m 5.0
                m'' = output m'        

            featureRequests m''   `shouldBe` 5.0
            deliveredFeatures m'' `shouldBe` 1.0
            codingTasks m''       `shouldBe` 4.0
            
        it "should output 0 if stock is 0" $ do
            let m   = setTeamCapacity initial 1.0
                m'  = input m 0.0
                m'' = output m'        

            featureRequests m''   `shouldBe` 0.0
            deliveredFeatures m'' `shouldBe` 0.0
            codingTasks m''       `shouldBe` 0.0
            
