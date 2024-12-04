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

checkLine :: [Int] -> Bool
checkLine is = valuesInRange && (valuesAreIncreasing || valuesAreDecreasing)
    where valuesInRange = compareValues is
          valuesAreIncreasing = allAscending is
          valuesAreDecreasing = allDescending is

solvePart1 :: String -> String
solvePart1 = show . length . filter id . map (checkLine . parseLine) . lines

solvePart2 :: String -> String
solvePart2 = id
