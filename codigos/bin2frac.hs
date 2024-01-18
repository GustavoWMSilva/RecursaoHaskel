bin2frac :: ([Int], [Int]) -> Double
bin2frac (intPart, fracPart) =
    let intDecimal = bincompl2dec2 intPart
        fracDecimal = fracList2dec fracPart 1
    in if head intPart == 1
       then intDecimal - fracDecimal
       else intDecimal + fracDecimal

fracList2dec :: [Int] -> Int -> Double
fracList2dec [] _ = 0
fracList2dec (x:xs) y = (fromIntegral x) / (2^y) + fracList2dec xs (y + 1)

bincompl2dec2 :: [Int] -> Double
bincompl2dec2 [] = 0
bincompl2dec2 (x:xs) = if x == 1
                     then -(fromIntegral(x * 2 ^(length xs))) + bincompl2decAux xs
                     else (fromIntegral(x * 2 ^(length xs))) + bincompl2decAux xs
                     
bincompl2decAux :: [Int] -> Double
bincompl2decAux [] = 0
bincompl2decAux (x : xs) = (fromIntegral(x * 2 ^(length xs))) + bincompl2decAux xs
