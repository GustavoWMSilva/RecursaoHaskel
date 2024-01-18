bin2dec :: [Int] -> Int
bin2dec [] = 0
bin2dec (x : xs) = (x * 2 ^(length xs)) + bin2dec xs