{-# OPTIONS_GHC -Wall #-}
module HW01 where

-- Exercise 1 -----------------------------------------

-- Get the last digit from a number
lastDigit :: Integer -> Integer
lastDigit = flip mod 10

-- Drop the last digit from a number
dropLastDigit :: Integer -> Integer
dropLastDigit = flip div 10

-- Exercise 2 -----------------------------------------

toRevDigits :: Integer -> [Integer]
toRevDigits n
  | n <= 0 = []
  | otherwise = (lastDigit n) : toRevDigits (dropLastDigit n)

-- Exercise 3 -----------------------------------------

-- Double every second number in a list starting on the left.
doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther ys = checkDouble 1 ys
  where checkDouble _ [] = []
        checkDouble n (x:xs)
          | n <= 0 = []
          | (n `mod` 2) == 0 = (x * 2): checkDouble ((n + 1)::Integer) xs
          | otherwise = x : checkDouble ((n + 1)::Integer) xs

-- Exercise 4 -----------------------------------------

-- Calculate the sum of all the digits in every Integer.
sumDigits :: [Integer] -> Integer
sumDigits = sum . map (\x -> if x >= 10 then (x `div` 10) + (x `mod` 10) else x)


-- Exercise 5 -----------------------------------------

-- Validate a credit card number using the above functions.
luhn :: Integer -> Bool
luhn n = 0 == mod ((sumDigits . doubleEveryOther . toRevDigits) n) 10

-- Exercise 6 -----------------------------------------

-- Towers of Hanoi for three pegs
type Peg = String
type Move = (Peg, Peg)

hanoi :: Integer -> Peg -> Peg -> Peg -> [Move]
hanoi n a b c
  | n <= 0 = []
  | n == 1 = [(a,b)]
  | otherwise = (hanoi (n -1) a c b) ++ [(a, b)] ++ (hanoi (n -1) c b a)
