{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Models where

import Data.Aeson (FromJSON, ToJSON)
import Data.Decimal
import Database.PostgreSQL.Simple.FromField
import Database.PostgreSQL.Simple.ToField
import GHC.Base (Double, Eq, Int, Ord, String, undefined, error)
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

instance FromJSON Transaction

instance FromJSON TransactionType

instance FromJSON User

instance ToJSON User

instance ToJSON Transaction

instance ToJSON TransactionType


data TransactionType = Debit | Credit
  deriving (Eq, Show, Generic)

newtype UserId = UserId {u_value :: Refined Positive Int}
  deriving (Eq, Ord, Generic, ToJSON, FromJSON, FromField, ToField, FromHttpApiData)

newtype TransactionId = TransactionId {t_value :: Int}
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, FromField, ToField, FromHttpApiData)

type Amount = Double

instance (FromField a, Predicate p a) => FromField (Refined p a)  where
  fromField f bs = fromRight (error "error on FromField") .  refine <$> fromField f bs

instance (ToField a, Predicate p a) => ToField (Refined p a) where
  toField v = toField $ unrefine v

instance (FromHttpApiData a, Predicate p a) => FromHttpApiData (Refined p a) where
  parseUrlPiece t = do
    r <- parseUrlPiece t
    mapLeft (\l -> T.pack (displayRefineException l)) $ refine r

instance Show UserId where
  show (UserId v) = show $ unrefine v

data User = User
  { id :: !UserId,
    name :: !String,
    lastName :: !String,
    amount :: !Amount
  }
  deriving (Eq, Show, Generic)

data Transaction = Transaction
  { id :: !TransactionId,
    userId :: !UserId,
    amount :: !Amount,
    transactionType :: !TransactionType
  }
  deriving (Eq, Show, Generic)

type AppState = TVar (Map UserId [TransactionAmount])

trxAmount :: Transaction -> TransactionAmount
trxAmount (Transaction _ _ amount trxType) = TransactionAmount amount trxType

userAmount :: User -> Amount
userAmount = amount

getUserId :: User -> UserId
getUserId = id

getTransactionId :: Transaction -> TransactionId
getTransactionId = id

calculatedTransactionAmount :: Integral i => TransactionAmount -> DecimalRaw i
calculatedTransactionAmount (TransactionAmount amount Credit) = realFracToDecimal 2 amount
calculatedTransactionAmount (TransactionAmount amount Debit) = realFracToDecimal 2 (- amount)

data TransactionAmount = TransactionAmount
  { amount :: !Amount,
    transactionType :: !TransactionType
  }
  deriving (Show, Generic)

instance Eq TransactionAmount where
  a == b = calculatedTransactionAmount a == calculatedTransactionAmount b
