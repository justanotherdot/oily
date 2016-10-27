module E35 where

import           Data.List.Ordered (minus)
import qualified Data.Map.Strict   as MS
import           Data.Maybe        (isNothing)

{-
The number, 197, is called a circular prime because all rotations of the digits: 197, 971, and 719, are themselves prime.

There are thirteen such primes below 100: 2, 3, 5, 7, 11, 13, 17, 31, 37, 71, 73, 79, and 97.

How many circular primes are there below one million?
-}

{-
Plan of attack:

* Generate a list of primes up to one million somehow. (sieve of eratosthenes, maybe.)
* Generate a function to 'rotate' a prime. If a prime has n digits, we perform n
  left rotations.
* Add the original numbers and rotations to a map iff the number and its
  rotations are all primes.
  + We check if a number is prime simply by looking it up in our map of primes.

-}


-- | eratos is a Sieve of Eratosthenes up to and including some bound n.
--   not very efficiently, however. TODO
eratos :: Integer -> [Integer]
eratos n = eratos' [2..n]
    where
      eratos' []     = []
      eratos' (p:xs) = p : eratos' (xs `minus` [p*p,p*p+p..n])

primesList :: [Integer]
primesList = eratos 1000000

primes :: MS.Map Integer Integer
primes = MS.fromList $ zip primesList primesList

rotateLeftBy :: Integer -> Int -> Integer
rotateLeftBy n k = rs
  where n' = show n
        rs = read (drop k n' ++ take k n')

rotateLeftAll :: Integer -> [Integer]
rotateLeftAll n = [ rotateLeftBy n k | k <- [0..(length s - 1)] ]
  where s = show n

insertMany :: MS.Map Integer Integer -> [Integer] -> MS.Map Integer Integer
insertMany = foldr (\k m -> MS.insert k 1 m)


solveIt = length $ foldr go MS.empty primesList
  where go p m = if allPrime then insertMany m rs else m
          where
            rs          = rotateLeftAll p
            allPrime    = all (`MS.member` primes) rs
