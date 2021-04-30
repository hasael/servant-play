{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}


module Domain.User where

import Data.Aeson (FromJSON, ToJSON)
import Data.Decimal
import Database.PostgreSQL.Simple.FromField ( FromField(fromField) )
import Database.PostgreSQL.Simple.ToField
import Database.PostgreSQL.Simple
import GHC.Base (Double, Eq, Int, Ord, String, undefined, error, IO)
import GHC.Float.ConversionUtils
import GHC.Generics
import GHC.Show 
import Data.Map
import GHC.Conc.Sync
import Servant
import Prelude as P (Float, Fractional, Integral, Real (toRational), realToFrac, (&&), (+), (<), (==), (>) , ($), (<$>) ,(.))
import Refined
import Data.Either
import Data.Either.Combinators (mapLeft)
import qualified Data.Text as T
import Control.Monad.Reader  
import Domain.Helper

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
mkUserId id =  UserId $ fromRight (error "invalid userId") $ refine id

withId :: User -> UserId -> User
withId user userId = User userId (name user) (lastName user) ((amount :: User -> Amount) user)

withAmount ::  Amount -> User -> User
withAmount amount user = User (getUserId user) (name user) (lastName user) amount
