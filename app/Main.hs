module Main (main) where

import qualified Data.Foldable as DF(mapM_)
import qualified Data.Functor as F((<&>))
import qualified Data.Map as Map(Map, fromList, lookup, toList)
import qualified System.Environment as S(getArgs)

import qualified Challenge00 as C00(solvePart1, solvePart2)
import qualified Challenge01 as C01(solvePart1, solvePart2)
import qualified Challenge02 as C02(solvePart1, solvePart2)
import qualified Challenge03 as C03(solvePart1, solvePart2)

solvers :: Map.Map String (String -> String, String -> String)
solvers = Map.fromList
  [ ("00", (C00.solvePart1, C00.solvePart2))
  , ("01", (C01.solvePart1, C01.solvePart2))
  , ("02", (C02.solvePart1, C02.solvePart2))
  , ("03", (C03.solvePart1, C03.solvePart2))
  ]

parseArgs :: [String] -> Maybe String
parseArgs (a:_) = Just a
parseArgs [] = Nothing

readInput :: String -> IO String
readInput challengeNumber =
  let fileName = "app/inputs/input-" ++ challengeNumber in
  readFile fileName

solveChallenge :: String -> IO ()
solveChallenge cn = do
  input <- readInput cn
  let ms = Map.lookup cn solvers
  case ms of
    Nothing -> return ()
    Just (s1, s2) -> do
      putStrLn $ s1 input
      putStrLn $ s2 input

solveAllChallenges :: () -> IO ()
solveAllChallenges () = do
  DF.mapM_ solveSingleChallenge $ Map.toList solvers
  where solveSingleChallenge (cn, (s1, s2)) = do
          input <- readInput cn
          putStrLn $ s1 input
          putStrLn $ s2 input

main :: IO ()
main = do
  challengeName <- S.getArgs F.<&> parseArgs
  case challengeName of
    Just cn -> solveChallenge cn
    Nothing -> solveAllChallenges ()
