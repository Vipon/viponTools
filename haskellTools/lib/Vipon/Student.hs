module Vipon.Student
  ( Student(..)
  , GradeLevel(..)
  ) where

import Vipon.Name(Name(..))

data GradeLevel = Freshman
                | Sophmore
                | Junior
                | Senior deriving (Eq,Ord,Enum,Show)

data Student = Student
  { studentId :: Int
  , gradeLevel :: GradeLevel
  , studentName :: Name
  } deriving (Show)

