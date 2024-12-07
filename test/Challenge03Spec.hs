module Challenge03Spec(spec) where

import Test.Hspec
import Challenge03(solvePart1, solvePart2)

spec :: Spec
spec = do
  describe "solvePart1" $ do
    it "correctly parses numbers" $ do
      let l = "e() from()mul(1,3)/whe"
      solvePart1 l `shouldBe` "3"

    it "multiplies those numbers" $ do
      let l = "e() from()mul(4,6)/whe"
      solvePart1 l `shouldBe` "24"

    it "sums up the results of multiple multiplications" $ do
      let l = "ere() from()mul(4,6)/when()mul(13,2)-mul"
      solvePart1 l `shouldBe` "50"

  describe "solvePart2" $ do
    it "ignores things after a don't() statement" $ do
      let l = "ere() from()mul(4,6)/don't()when()mul(13,2)-mul"
      solvePart2 l `shouldBe` "24"

    it "stops ignoring things after a do() statement" $ do
      let l = "ere() from()mul(4,6)/don't()when()mul(13,2)-muldo()mul(9,2)"
      solvePart2 l `shouldBe` "42"
