{-# OPTIONS_GHC -Wall #-}
module HW02 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches secretCode guess 
  | length secretCode /= length guess = error "do not match"
  | otherwise = length (filter (\(a,b) -> a == b) (zip secretCode guess))

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors code = countColor colors
  where 
    countColor [] = []
    countColor (peg:pegs) = (length $ filter (== peg) code) : (countColor pegs)

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches secret guess
  | length secret /= length guess = error "do not match"
  | otherwise = sum (map (\(a,b) -> if a <= b then a else b) countColorPairs)
    where
      countColorPairs = zip (countColors secret) (countColors guess)

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove secret guess = Move guess exM (matches secret guess - exM)
  where
    exM = exactMatches secret guess

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent (Move code1 exact nonExact) code2 = (Move code1 exact nonExact) == (getMove code2 code1)

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes move = filter (\ code -> isConsistent move code)

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes n  
  | n <= 0 = []
  | n == 1 = map (\a -> [a]) colors
  | otherwise = concatMap (\a -> map (\b -> a:b) $ allCodes (n - 1)) colors
    

-- Exercise 7 -----------------------------------------

solveHelper:: [Code] -> Code -> [Move] -> [Move]
solveHelper xs y zs 
  | length xs == 1 = zs
  | otherwise =  solveHelper filteredMove y (move:zs)
  where
  move = getMove y (head xs) 
  filteredMove = filter (isConsistent move) xs 
  

solve :: Code -> [Move]
solve xs = reverse $ solveHelper codes xs []
  where codes = allCodes $ length xs

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
