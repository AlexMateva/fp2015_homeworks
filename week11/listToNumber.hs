-- Converts a list of digits to its corresponding number
listToNumber :: [Int] -> Int
listToNumber [] = 0
listToNumber (x:xs) =
    x * (10 ^ length xs) + listToNumber xs

-- Checks if xs is a suffix of ys
suffix :: [Int] -> [Int] -> Bool
suffix xs ys
    |null ys = False
    |listToNumber xs == listToNumber ys = True
    |otherwise = suffix xs (tail ys)

-- Counts occurences of each element of xs in ys
occurrences :: [Int] -> [Int] -> [Int]
occurrences [] _ = []
occurrences _ [] = []
occurrences (x:xs) ys = length(filter (== x) ys) : occurrences xs ys

-- Removes element at given index
removeAt :: [Int] -> Int -> [Int]
removeAt [] _ = []
removeAt xs i
    |i > length xs || i < 0 = []
    |otherwise = take i xs ++ drop (i + 1) xs
