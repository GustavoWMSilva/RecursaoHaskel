dec2bin :: Int -> Int -> [Int]
dec2bin _ 0 = []
dec2bin x y = dec2bin (div x 2) (y-1) ++ [mod x 2]

dec2bincompl :: Int -> Int -> [Int]
dec2bincompl _ 0 = []
dec2bincompl x y = if x >= 0 then dec2bin x y
                   else [1] ++ dec2bincomplaux (-(2^(y-1))) x (y-1)


dec2bincomplaux :: Int -> Int -> Int -> [Int]
dec2bincomplaux _ _ 0 = []
dec2bincomplaux w x y = if (w == x) then [0] ++ dec2bincomplaux w w (y-1)
                        else if (w + (2^(y-1))) <= x then [1] ++ dec2bincomplaux (w + (2^(y-1))) x (y-1)
                        else [0] ++ dec2bincomplaux w x (y-1)