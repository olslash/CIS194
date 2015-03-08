toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0    = []
  | otherwise = toDigits (n `div` 10) ++ [n `mod` 10]

toDigitsRev :: Integer -> [Integer]
toDigitsRev n = reverse (toDigits n)

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther n = reverse $ zipWith (*) (reverse n) (cycle [1, 2])

sumDigits :: [Integer] -> Integer
sumDigits [] = 0
sumDigits [x]
  | length digits > 1 = sumDigits digits
  | otherwise = x
  where digits = toDigits x
sumDigits (x:xs) = sumDigits (toDigits x) + (sumDigits xs)


validate :: Integer -> Bool
validate n = ((`rem` 10) . sumDigits . doubleEveryOther . toDigits) n == 0

