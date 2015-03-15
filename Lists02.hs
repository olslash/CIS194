module Lists02
(  ) where

data Peg = Red | Green | Blue | Yellow | Orange | Purple
        deriving(Show, Eq, Ord)

type Code = [Peg]

data Move = Code Int Int
        deriving(Show, Eq)

exactMatches :: Code -> Code -> Int
exactMatches [] [] = 0
exactMatches (actual:xs) (guess:ys)
        | actual == guess = 1 + exactMatches xs ys
        | otherwise       = exactMatches xs ys

count :: Code -> Peg -> Int
count [] _ = 0
count (x:xs) peg
    | x == peg  = 1 + count xs peg
    | otherwise = count xs peg

colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

countColors :: Code -> [Int]
countColors code = map (count code) colors
-- pointfree would be countColors = flip map colors . count

matches:: Code -> Code -> Int
matches secret guess = sum $ zipWith min (countColors secret) (countColors guess)
