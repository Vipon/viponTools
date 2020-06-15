module Vipon.Primes
  ( sieve
  , isPrime
  , primeFactors
  ) where

import Data.Maybe(Maybe(..))

sieve :: [Int] -> [Int]
sieve [] = []
sieve (nextPrime : rest) = nextPrime : sieve noFactors
  where noFactors = filter ((/= 0) . (`mod` nextPrime)) rest

isPrime :: Int -> Maybe Bool
isPrime n | n < 2 = Nothing
          | otherwise = Just (n `elem` primes)
  where primes = sieve [2 .. n]

primeFactorsLoop :: Int -> [Int] -> [Int]
primeFactorsLoop 0 _ = []
primeFactorsLoop n [] = []
primeFactorsLoop n (next:primes) =
  if n `mod` next == 0
    then next : primeFactorsLoop (n `div` next) (next:primes)
    else primeFactorsLoop n primes

primeFactors :: Int -> Maybe [Int]
primeFactors n | n < 2 = Nothing
               | n >= length primes = Nothing
               | otherwise = Just (primeFactorsLoop n primesLessThanN)
  where
    primes = sieve [2 .. n]
    primesLessThanN = filter (<= n) primes

