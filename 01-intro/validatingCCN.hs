toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev


toDigitsRev :: Integer -> [Integer]
toDigitsRev x  
    | x < 1 = []
    | otherwise  = (x `mod` 10) : toDigitsRev (x `div` 10)


doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther l = if (length l) `mod` 2 == 0 then doubleFirst l else noDouble l

doubleFirst :: [Integer] -> [Integer]
doubleFirst [] = []
doubleFirst (x: xs) = (2 * x) : (noDouble xs)

noDouble :: [Integer] -> [Integer]
noDouble [] = []
noDouble (x: xs) = x : (doubleFirst xs)

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits (x: xs) = (if x < 10 then x else (div x 10) + (mod x 10)) + sumDigits(xs)

validate :: Integer -> Bool
validate nums = (sumDigits (doubleEveryOther (toDigits nums))) `mod` 10 == 0