module Vipon.Text
  ( isPalindrome
  ) where

isPalindrome :: String -> Bool
isPalindrome t = t == reverse t

