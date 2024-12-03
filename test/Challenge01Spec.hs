module Challenge01Spec where

import Test.Hspec
import Challenge01(solve)

spec :: Spec
spec = do
    describe "Challenge01" $ do
        it "determines distances from one line" $ do
            solve "1   2" `shouldBe` "1"

        it "determines distances from two lines" $ do
            solve "3   5\n8   10" `shouldBe` "4"

        it "determines distance when left side is larger than right side" $ do
            solve "8   4" `shouldBe` "4"

        it "determines distance with longer list" $ do
            let l = "3   4\n\
                    \4   3\n\
                    \2   5\n\
                    \1   3\n\
                    \3   9\n\
                    \3   3"
            solve l `shouldBe` "11"
