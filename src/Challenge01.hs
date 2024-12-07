module Challenge01(solvePart1, solvePart2) where

import Text.Regex.TDFA
import qualified Data.List as DL(sort)
import qualified Data.Bifunctor as BF(bimap)

parseLine :: String -> (Int, Int)
parseLine l =
  let (first, _, third) = l =~ "   " :: (String, String, String)
  in (read first, read third)

toPairOfLists :: [(a, b)] -> ([a], [b])
toPairOfLists [] = ([], [])
toPairOfLists ((a, b):xs) = (a:as, b:bs)
  where (as, bs) = toPairOfLists xs

toListOfPairs :: ([a], [b]) -> [(a, b)]
toListOfPairs ([], []) = []
toListOfPairs (_:_, []) = []
toListOfPairs ([], _:_) = []
toListOfPairs (a:as, b:bs) = (a, b):rest
  where rest = toListOfPairs (as, bs)

absSum :: (Int, Int) -> Int
absSum (a, b) = abs $ a - b

buildSimilarityScore :: ([Int], [Int]) -> Int
buildSimilarityScore (a, b) = foldr folder 0 a
  where folder e agg =
          let count = (length . filter (== e)) b
              score = e * count
              in agg + score

solvePart1 :: String -> Int
solvePart1 = sum . map absSum . toListOfPairs . BF.bimap DL.sort DL.sort . toPairOfLists . map parseLine . lines

solvePart2 :: String -> Int
solvePart2 = buildSimilarityScore . toPairOfLists . map parseLine . lines
