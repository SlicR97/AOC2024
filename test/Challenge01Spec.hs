module Challenge01Spec(spec) where

import Test.Hspec
import Challenge01(solvePart1, solvePart2)

spec :: Spec
spec = do
  describe "solvePart1" $ do
    it "determines distances from one line" $ do
        solvePart1 "1   2" `shouldBe` "1"

    it "determines distances from two lines" $ do
      solvePart1 "3   5\n8   10" `shouldBe` "4"

    it "determines distance when left side is larger than right side" $ do
      solvePart1 "8   4" `shouldBe` "4"

    it "determines distance with longer list" $ do
      let l = "3   4\n\
              \4   3\n\
              \2   5\n\
              \1   3\n\
              \3   9\n\
              \3   3"

      solvePart1 l `shouldBe` "11"

  describe "solvePart2" $ do
    it "returns 0 for a list of 2 different values" $ do
      solvePart2 "3   2" `shouldBe` "0"

    it "returns 1 for a list of 2 1s" $ do
      solvePart2 "1   1" `shouldBe` "1"

    it "returns 2 for a list of 2 2s" $ do
      solvePart2 "2   2" `shouldBe` "2"

    it "returns 3 for a list of 2 1s and 2 2s" $ do
      solvePart2 "1   1\n\
                 \2   2" `shouldBe` "3"

    it "returns 4 for a list with one 2 on the left and 2 2s on the right" $ do
      solvePart2 "2   2\n\
                 \3   2" `shouldBe` "4"

    it "returns 8 for a list with two 3s and one 2" $ do
      solvePart2 "2   3\n\
                 \3   2\n\
                 \1   3" `shouldBe` "8"
