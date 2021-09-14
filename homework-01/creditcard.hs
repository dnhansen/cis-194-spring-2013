-- Exercise 1

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0    = []
  | otherwise = (n `mod` 10) : toDigitsRev (n `div` 10)

toDigits :: Integer -> [Integer]
toDigits = reverse . toDigitsRev


-- Exercise 2

doubleFromLeft :: [Integer] -> [Integer]
doubleFromLeft []       = []
doubleFromLeft [x]      = [2*x]
doubleFromLeft (x:y:xs) = 2*x : y : doubleFromLeft xs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther xs
  | xs == []         = []
  | even (length xs) = doubleFromLeft xs
  | otherwise        = (head xs) : doubleFromLeft (tail xs)


-- Exercise 3

sumDigits :: [Integer] -> Integer
sumDigits = sum . map (\n -> if n < 10 then n else n `mod` 10 + 1)


-- Exercise 4

validate :: Integer -> Bool
validate n = (sumDigits . doubleEveryOther . toDigits) n `mod` 10 == 0