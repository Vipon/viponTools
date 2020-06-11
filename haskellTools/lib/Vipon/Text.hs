module Vipon.Text
  ( stripWhiteSpace
  , stripPunctuation
  , toLower
  , palindromePreprocess
  , isPalindrome
  ) where

import qualified Data.Char as Char(toLower, isSpace, isPunctuation)

stripWhiteSpace :: String -> String
stripWhiteSpace = filter (not . Char.isSpace)

stripPunctuation :: String -> String
stripPunctuation = filter (not . Char.isPunctuation)

toLower :: String -> String
toLower = map Char.toLower

palindromePreprocess :: String -> String
palindromePreprocess = stripWhiteSpace . stripPunctuation . toLower

isPalindrome :: String -> Bool
isPalindrome t = cleanText == reverse cleanText
  where cleanText = palindromePreprocess t

