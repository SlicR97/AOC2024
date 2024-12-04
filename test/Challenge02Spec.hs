module Challenge02Spec(spec) where

import Test.Hspec
import Challenge02(solvePart1)

spec :: Spec
spec = do
    describe "Challenge02" $ do
        it "determines a line as unsafe if it consists of 2 values and the second value increases by more than 3" $ do
            solvePart1 "10 15" `shouldBe` "0"

        it "determines a line as safe if it consists of 2 values and the second value increases by 1" $ do
            solvePart1 "1 2" `shouldBe` "1"

        it "determines a line as safe it it consists of 2 values and the second value increases by 2" $ do
            solvePart1 "1 3" `shouldBe` "1"

        it "determines a line as safe if it consists of 2 values and the second value decreases by 1" $ do
            solvePart1 "2 1" `shouldBe` "1"

        it "determines a line as safe it it consists of 2 values and the second value decreases by 2" $ do
            solvePart1 "3 1" `shouldBe` "1"

        it "determines a line as unsafe if it consists of 2 values and the second value decreases by more than 3" $ do
            solvePart1 "8 4" `shouldBe` "0"

        it "returns the number of safe lines" $ do
            let l = "3 1\n\
                    \1 2"
            
            solvePart1 l `shouldBe` "2"

        it "determines a line as safe if it consists of 3 values, with the first two being in the range and the last two being in the range" $ do
            solvePart1 "1 3 4" `shouldBe` "1"

        it "determines a line as unsafe if it consists of 3 values, with the first two being in the range and the last two not" $ do
            solvePart1 "1 3 8" `shouldBe` "0"

        it "determines a list as unsafe if not all values are either increasing or decreasing" $ do
            solvePart1 "1 3 2" `shouldBe` "0"

        it "determines a line as unsafe if it has two of the same number next to each other" $ do
            solvePart1 "4 4" `shouldBe` "0"

        it "determines the correct number of safe lines" $ do
            let l = "1 3\n\
                    \1 4\n\
                    \1 5\n\
                    \2 1\n\
                    \8 3\n\
                    \4 4\n\
                    \2 4 4\n\
                    \1 3 5\n\
                    \8 6 4\n\
                    \1 3 8\n\
                    \8 6 1\n\
                    \1 3 2\n\
                    \3 2 4"
            
            solvePart1 l `shouldBe` "5"
