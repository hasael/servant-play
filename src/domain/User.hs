{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Domain.User where

import Data.Aeson (FromJSON, ToJSON)
import Data.Either
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField (FromField (fromField))
import Database.PostgreSQL.Simple.ToField
import Domain.Helper
import GHC.Base (Eq, Int, Ord, String, error)
import GHC.Generics
import GHC.Show
import Refined
import Servant
import Prelude (($))

instance FromJSON User

instance ToJSON User

instance FromRow User

newtype UserId = UserId {u_value :: Refined Positive Int}
  deriving (Eq, Ord, Generic, ToJSON, FromJSON, FromField, ToField, FromHttpApiData)

instance Show UserId where
  show (UserId v) = show $ unrefine v

data User = User
  { id :: !UserId,
    name :: !String,
    lastName :: !String,
    amount :: !Amount
  }
  deriving (Eq, Show, Generic)

userAmount :: User -> Amount
userAmount = amount

getUserId :: User -> UserId
getUserId = id

mkUserId :: Int -> UserId
mkUserId id = UserId $ fromRight (error "invalid userId") $ refine id

withId :: User -> UserId -> User
withId user userId = User userId (name user) (lastName user) ((amount :: User -> Amount) user)

withAmount :: Amount -> User -> User
withAmount amount user = User (getUserId user) (name user) (lastName user) amount
