module Vipon.Name
  ( Name(..)
  ) where

data Name = Name
  { firstName :: String
  , lastName :: String
  }

instance Show Name where
  show (Name first last) = mconcat [first, " ", last]

