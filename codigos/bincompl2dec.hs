bincompl2dec :: [Int] -> Int
bincompl2dec [] = 0
bincompl2dec (x:xs) = if x == 1
                     then -(x * 2 ^(length xs)) + bincompl2decAux xs
                     else (x * 2 ^(length xs)) + bincompl2decAux xs
                     
bincompl2decAux :: [Int] -> Int
bincompl2decAux [] = 0
bincompl2decAux (x : xs) = (x * 2 ^(length xs)) + bincompl2decAux xs