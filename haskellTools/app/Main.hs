module Main where

import Vipon.Text

main :: IO ()
main = do
  print "Enter a word and I'll let you know if it's a palindrome!"
  text <- getLine
  let response = if isPalindrome text
                  then "it is!"
                  else "it's not!"
  print response

