module Candidate
  ( Grade
  , Degree
  , Candidate
  , viable
  , readInt
  , readGrade
  , readDegree
  , readCandidate
  , assessCandidateIO
  ) where

import qualified Data.Map as Map

data Grade = F
           | D
           | C
           | B
           | A
           deriving (Eq, Ord, Enum, Show, Read)

data Degree = HS
            | BA
            | MS
            | PhD
            deriving (Eq, Ord, Enum, Show, Read)

data Candidate = Candidate
  { candidateId :: Int
  , codeReview  :: Grade
  , cultureFit  :: Grade
  , education   :: Degree
  } deriving (Show)

viable :: Candidate -> Bool
viable candidate = all (== True) tests
  where
    tests =
      [ codeReview candidate > B
      , cultureFit candidate > C
      , education candidate >= MS
      ]

readInt :: IO Int
readInt = getLine >>= (pure . read)

readGrade :: IO Grade
readGrade = getLine >>= (pure . read)

readDegree :: IO Degree
readDegree = getLine >>= (pure . read)

readCandidate :: IO Candidate
readCandidate = do
  putStr "Enter id: "
  id <- readInt
  putStr "Enter code grade: "
  codeGrade <- readGrade
  putStr "Enter culture fit grade: "
  cultureGrade <- readGrade
  putStr "Enter education: "
  degree <- readDegree
  pure (Candidate { candidateId = id
                  , codeReview  = codeGrade
                  , cultureFit  = cultureGrade
                  , education   = degree
                  })

passStatement :: Bool -> String
passStatement passed = if passed
  then "passed"
  else "failed"

assessCandidateIO :: IO String
assessCandidateIO = do
  candidate <- readCandidate
  let passed = viable candidate
  let statement = if passed
                    then "passed"
                    else "failed"
  pure statement

candidate1 :: Candidate
candidate1 = Candidate
  { candidateId = 1
  , codeReview = A
  , cultureFit = A
  , education = BA
  }

candidate2 :: Candidate
candidate2 = Candidate
  { candidateId = 2
  , codeReview = C
  , cultureFit = A
  , education = PhD
  }

candidate3 :: Candidate
candidate3 = Candidate
  { candidateId = 3
  , codeReview = A
  , cultureFit = B
  , education = MS
  }

candidates :: [Candidate]
candidates = [ candidate1
             , candidate2
             , candidate3
             ]

candidateDB :: Map.Map Int Candidate
candidateDB = Map.fromList [ (1, candidate1)
                           , (2, candidate2)
                           , (3, candidate3)
                           ]

assessCandidateMaybe :: Int -> Maybe String
assessCandidateMaybe cId = do
  candidate <- Map.lookup cId candidateDB
  let passed = viable candidate
  pure (passStatement passed)

assessCandidateList :: [Candidate] -> [String]
assessCandidateList candidates = do
  candidate <- candidates
  let passed = viable candidate
  pure (passStatement passed)

assessCandidatesNotMonad :: [Candidate] -> [String]
assessCandidatesNotMonad candidates =
  map (\x -> if x
               then "passed"
               else "failed") passed
  where passed = map viable candidates

assessCandidate :: Monad m => m Candidate -> m String
assessCandidate candidates = do
  candidate <- candidates
  let passed = viable candidate
  pure (passStatement passed)

