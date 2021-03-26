{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Models where

import Data.Aeson (FromJSON, ToJSON)
import Data.Decimal
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField
import GHC.Base (Double, Eq, Int, Ord, String)
import GHC.Float.ConversionUtils
import GHC.Generics
import GHC.Show (Show)
import Data.Map
import GHC.Conc.Sync
import Servant
import Prelude (Float, Fractional, Integral, Real (toRational), realToFrac, (&&), (+), (<), (==), (>))

instance FromJSON Transaction

instance FromJSON TransactionType

instance FromJSON User

instance ToJSON User

instance ToJSON Transaction

instance ToJSON TransactionType

data TransactionType = Debit | Credit
  deriving (Eq, Show, Generic)

newtype UserId = UserId {u_value :: Int}
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, FromField, ToField, FromHttpApiData)

newtype TransactionId = TransactionId {t_value :: Int}
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, FromField, ToField, FromHttpApiData)

type Amount = Double

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
