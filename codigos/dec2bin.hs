dec2bin :: Int -> Int -> [Int]
dec2bin _ 0 = []
dec2bin x y = dec2bin (div x 2) (y-1) ++ [mod x 2]