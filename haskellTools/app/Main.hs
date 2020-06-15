{-# LANGUAGE OverloadedStrings #-}

module Main where

import Vipon.Text (isPalindrome)
import Data.Text.IO as TIO(putStrLn, getLine)

main :: IO ()
main = do
  TIO.putStrLn "Enter a word and I'll let you know if it's a palindrome!"
  text <- TIO.getLine
  let response = if isPalindrome text
                  then "it is!"
                  else "it's not!"
  TIO.putStrLn response

