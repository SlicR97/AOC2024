module Challenge01(solve) where

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
toListOfPairs (a:as, b:bs) = (a, b):rest
  where rest = toListOfPairs (as, bs)

absSum :: (Int, Int) -> Int
absSum (a, b) = abs $ a - b

solve :: String -> String
solve = show . sum . map absSum . toListOfPairs . BF.bimap DL.sort DL.sort . toPairOfLists . map parseLine . lines
