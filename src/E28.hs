module E28 where

{-
Starting with the number 1 and moving to the right in a clockwise direction a 5
by 5 spiral is formed as follows:

21 22 23 24 25
20  7  8  9 10
19  6  1  2 11
18  5  4  3 12
17 16 15 14 13

It can be verified that the sum of the numbers on the diagonals is 101.

What is the sum of the numbers on the diagonals in a 1001 by 1001 spiral formed
in the same way?
-}

-- | We notice that each layer has the sum of m^2-k(m-1) for k=0,1,2
-- Then we simply have to sum each layer.

-- Sum a single layer of square matrix of odd dimension `m'
sumLayer :: Integer -> Integer
sumLayer m
  | m == 1    = 1
  | even m    = 0
  | otherwise = sum $ map (\k -> m^2-k*(m-1)) [0..3]

-- Sum a square matrix of odd dimension `m'
sumSquareMatrix :: Integer -> Integer
sumSquareMatrix m
  | m == 1    = sumLayer 1
  | odd m     = sum $ map sumLayer [1,3..m]
  | otherwise = 0

solveIt :: Integer
solveIt = sumSquareMatrix 1001
