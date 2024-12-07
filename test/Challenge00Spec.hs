module Challenge00Spec(spec) where

import Test.Hspec
import Challenge00(solvePart1, solvePart2)

spec :: Spec
spec = do
  describe "solvePart1" $ do
    it "returns 1" $ do
        solvePart1 "" `shouldBe` 1

  describe "solvePart2" $ do
    it "returns 2" $ do
      solvePart2 "" `shouldBe` 2
