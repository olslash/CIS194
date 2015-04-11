{-# OPTIONS_GHC -Wall #-}
module Lists02
( Move(..), Code, Peg ) where

data Peg = Red | Green | Blue | Yellow | Orange | Purple
        deriving(Show, Eq, Ord)

type Code = [Peg]

data Move = Move Code Int Int
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

matches :: Code -> Code -> Int
matches secret guess = sum $ zipWith min (countColors secret) (countColors guess)

getMove :: Code -> Code -> Move
getMove secret guess = Move guess exact nonexact
    where 
        exact    = exactMatches secret guess
        nonexact = (matches secret guess) - exact

-- does the guess inside the move have the same # of exact and non-exact matches with the provided code as it did with the actual secret
-- look into Move and compare its 2nd and 3rd args against getMove Code (move's code)
isConsistent :: Move -> Code -> Bool
isConsistent compare@(Move code _ _) maybeSecret = getMove maybeSecret code  == compare

filterCodes :: Move -> [Code] -> [Code]
filterCodes move codes = filter (isConsistent move) codes

codesPlusOne :: [Code] -> [Code]
codesPlusOne (code:codes) = [concatMap (\x -> [x]) colors]

-- add the first element of colors to the first element 
-- [[red], [green], [blue]]
-- [[red, red], [red, green], [red, blue], 
-- [green red], [green green], [green blue], 
-- [blue red], [blue green], [blue blue]]


-- takes in all the codes of length n − 1 and uses it to produce all codes of length n
-- adds each color to the end of the input, adding that to the output


-- allCodes :: Int -> [Code]
-- allCodes 0 = []
-- allCodes n = : allCodes n - 1

