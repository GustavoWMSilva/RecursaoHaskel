frac2bin :: Double -> ([Int], [Int])
frac2bin num =
  let
    (intPart, fracPart) = properFraction num
    intBin = dec2bincompl intPart 16
    fracBin = frac2binAux (abs fracPart) 0 16
  in
    (intBin, fracBin)

frac2binAux :: Double -> Int -> Int -> [Int]
frac2binAux _ 16 0 = []
frac2binAux frac n remainingBits =
  let
    newFrac = frac*2
    bit = if 1 <= newFrac then 1 else 0
    (intPart, fracPart) = properFraction newFrac
    nFrac = fracPart
  in
    bit : frac2binAux nFrac (n + 1) (remainingBits - 1)


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


