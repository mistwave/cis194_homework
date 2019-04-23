toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev


toDigitsRev :: Integer -> [Integer]
toDigitsRev x  
    | x < 1 = []
    | otherwise  = (x `mod` 10) : toDigitsRev (x `div` 10)