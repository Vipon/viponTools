module Vipon.Course
  ( Course(..)
  ) where

data Course = Course
  { courseId :: Int
  , courseTitle :: String
  , teacher :: Int
  } deriving (Show)

