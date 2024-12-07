module Challenge03(solvePart1, solvePart2) where

import Text.Regex.TDFA ((=~))

data Statement
  = DoStmt
  | DontStmt
  | MulStmt Int Int

parseInput1 :: String -> [(Int, Int)]
parseInput1 l =
  let matchGroups = l =~ "mul\\(([0-9]+),([0-9]+)\\)" :: [[String]]
  in map readGrp matchGroups
  where readGrp ss = (read $ ss !! 1, read $ ss !! 2)

parseInput2 :: String -> [Statement]
parseInput2 l =
  let matchGroups = l =~ "mul\\(([0-9]+),([0-9]+)\\)|do\\(\\)|don't\\(\\)" :: [[String]]
  in map parseGrp matchGroups
  where parseGrp ss =
          case head ss of
            "do()" -> DoStmt
            "don't()" -> DontStmt
            _ -> MulStmt (read $ ss !! 1) (read $ ss !! 2)

interpretInput2 :: [Statement] -> Int
interpretInput2 = doInterpretInput 0 True
  where doInterpretInput x _ (DoStmt:r) = doInterpretInput x True r
        doInterpretInput x _ (DontStmt:r) = doInterpretInput x False r
        doInterpretInput x True ((MulStmt a b):r) = doInterpretInput (x + (a * b)) True r
        doInterpretInput x False ((MulStmt _ _):r) = doInterpretInput x False r
        doInterpretInput x _ [] = x

solvePart1 :: String -> Int
solvePart1 = sum . map mul . parseInput1
  where mul (a, b) = a * b

solvePart2 :: String -> Int
solvePart2 = interpretInput2 . parseInput2
