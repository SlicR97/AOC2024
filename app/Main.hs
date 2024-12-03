module Main (main) where

import qualified Data.Foldable as DF(mapM_)
import qualified Data.Functor as F((<&>))
import qualified Data.Map as Map(Map, fromList, lookup, toList)
import qualified System.Environment as S(getArgs)

import qualified Challenge00 as C00(solve)
import qualified Challenge01 as C01(solve)

solvers :: Map.Map String (String -> String)
solvers = Map.fromList
    [ ("00", C00.solve)
    , ("01", C01.solve)
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
        Just s -> putStrLn $ s input

solveAllChallenges :: () -> IO ()
solveAllChallenges () = do
    DF.mapM_ solveSingleChallenge $ Map.toList solvers
    where solveSingleChallenge (cn, solver) = do
            input <- readInput cn
            putStrLn $ solver input

main :: IO ()
main = do
    challengeName <- S.getArgs F.<&> parseArgs
    case challengeName of
        Just cn -> solveChallenge cn
        Nothing -> solveAllChallenges ()
