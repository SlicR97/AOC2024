module Challenge02(solvePart1, solvePart2) where

parseLine :: String -> [Int]
parseLine = map read . words

compareValues :: [Int] -> Bool
compareValues (a:b:xs) =
  let distance = abs (a - b)
  in distance > 0 && distance < 4 && compareValues (b:xs)
compareValues [_] = True
compareValues [] = True

allAscending :: [Int] -> Bool
allAscending [] = True
allAscending [_] = True
allAscending (a:b:xs) = a <= b && allAscending (b:xs)

allDescending :: [Int] -> Bool
allDescending [] = True
allDescending [_] = True
allDescending (a:b:xs) = a >= b && allDescending (b:xs)

dropElement :: Int -> [a] -> [a]
dropElement _ [] = []
dropElement i (a:as)
  | i == 0 = as
  | otherwise = a : dropElement (i - 1) as

checkLine :: [Int] -> Bool
checkLine is = valuesInRange && (valuesAreIncreasing || valuesAreDecreasing)
  where valuesInRange = compareValues is
        valuesAreIncreasing = allAscending is
        valuesAreDecreasing = allDescending is

checkLine2 :: [Int] -> Bool
checkLine2 ls = any checkLine ([ dropElement x ls | x <- [0..(length ls - 1)] ])

solvePart1 :: String -> String
solvePart1 = show . length . filter id . map (checkLine . parseLine) . lines

solvePart2 :: String -> String
solvePart2 = show . length . filter id . map (checkLine2 . parseLine) . lines
