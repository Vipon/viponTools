module Test.Vipon.Primes
  ( testPrimes
  ) where

import Test.QuickCheck

import Data.Maybe(Maybe(..), isJust, fromJust)
import Vipon.Primes(isPrime, primeFactors)

prop_validPrimesOnly val =
  if val < 2
    then result == Nothing
    else isJust result
  where result = isPrime val

prop_primesArePrime val =
  if result == Just True
    then length divisors == 0
    else True
  where
    result = isPrime val
    divisors = filter ((== 0) . (val `mod`)) [2 .. (val - 1)]

prop_nonPrimesAreComposite val =
  if result == Just False
    then length divisors > 0
    else True
  where
    result = isPrime val
    divisors = filter ((== 0) . (val `mod`)) [2 .. (val - 1)]

prop_factorsMakeOriginal val =
  if result == Nothing
    then True
    else product (fromJust result) == val
  where result = primeFactors val

prop_allFactorsPrime val =
  if result == Nothing
    then True
    else all (== Just True) resultsPrime
  where
    result = primeFactors val
    resultsPrime = map isPrime (fromJust result)

testPrimes :: IO ()
testPrimes = do
  quickCheck prop_validPrimesOnly
  quickCheckWith stdArgs { maxSuccess = 1000} prop_primesArePrime
  quickCheckWith stdArgs { maxSuccess = 1000} prop_nonPrimesAreComposite
  quickCheck prop_factorsMakeOriginal
  quickCheck prop_allFactorsPrime

