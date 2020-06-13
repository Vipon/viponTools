{-# LANGUAGE OverloadedStrings #-}

module Vipon.Text
  ( stripWhiteSpace
  , stripPunctuation
  , toLower
  , palindromePreprocess
  , isPalindrome
  ) where

import qualified Data.Text as T(Text, filter, reverse, toLower)
import qualified Data.Char as Char(isSpace, isPunctuation)

stripWhiteSpace :: T.Text -> T.Text
stripWhiteSpace = T.filter (not . Char.isSpace)

stripPunctuation :: T.Text -> T.Text
stripPunctuation = T.filter (not . Char.isPunctuation)

toLower :: T.Text -> T.Text
toLower = T.toLower

palindromePreprocess :: T.Text -> T.Text
palindromePreprocess = stripWhiteSpace . stripPunctuation . toLower

isPalindrome :: T.Text -> Bool
isPalindrome t = cleanText == T.reverse cleanText
  where cleanText = palindromePreprocess t

