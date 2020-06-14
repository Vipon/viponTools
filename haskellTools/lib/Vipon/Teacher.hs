module Vipon.Teacher
  ( Teacher(..)
  ) where

import Vipon.Name(Name(..))

data Teacher = Teacher
  { teacherId :: Int
  , teacherName :: Name
  } deriving (Show)


