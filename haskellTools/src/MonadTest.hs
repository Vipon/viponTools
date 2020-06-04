module MonadTest(
  ) where

import qualified Data.Map as Map (Map, fromList, lookup)
import           Data.Maybe (Maybe(..))

type UserName = String
type UserId = Int
type UserCredits = Int

userNameDb :: Map.Map UserId UserName
userNameDb = Map.fromList
  [ (1,"nYarlathoTep")
  , (2,"KINGinYELLOW")
  , (3,"dagon1997")
  , (4,"rcarter1919")
  , (5,"xCTHULHUx")
  , (6,"yogSOThoth")
  ]

creditsDb :: Map.Map UserName UserCredits
creditsDb = Map.fromList
  [ ("nYarlathoTep",2000)
  , ("KINGinYELLOW",15000)
  , ("dagon1997",300)
  , ("rcarter1919",12)
  , ("xCTHULHUx",50000)
  , ("yogSOThoth",150000)
  ]

creditsFromId :: UserId -> Maybe UserCredits
creditsFromId id = lookupUserName id >>= lookupCredits

lookupUserName :: UserId -> Maybe UserName
lookupUserName id = Map.lookup id userNameDb

lookupCredits :: UserName -> Maybe UserCredits
lookupCredits name = Map.lookup name creditsDb

altLookupCredits :: Maybe UserName -> Maybe UserCredits
altLookupCredits Nothing = Nothing
altLookupCredits (Just name) = Map.lookup name creditsDb

altCreditsFromId :: UserId -> Maybe UserCredits
altCreditsFromId = altLookupCredits . lookupUserName

