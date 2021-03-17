{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
module Models where

import Data.Aeson (FromJSON, ToJSON)
import Database.PostgreSQL.Simple.FromField (FromField)
import Database.PostgreSQL.Simple.ToField
import GHC.Base (Double, Int, Ord, String, Eq)
import GHC.Generics
import GHC.Show (Show)
import GHC.Float.ConversionUtils
import Servant
import Prelude ((+), (<) , (>), Float, Fractional, realToFrac, Real (toRational), Integral, (==), (&&))
import Data.Decimal


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
  deriving ( Show, Generic)

transactionAmount :: Transaction -> Amount
transactionAmount = amount

trxAmount :: Transaction -> TransactionAmount
trxAmount (Transaction _ _ amount trxType) = TransactionAmount amount trxType

calculatedtransactionAmount :: Integral i => Transaction -> DecimalRaw i
calculatedtransactionAmount (Transaction _ _ amount Credit) = realFracToDecimal 2 amount
calculatedtransactionAmount (Transaction _ _ amount Debit) = realFracToDecimal 2 (-amount)

userAmount :: User -> Amount
userAmount = amount

getUserId :: User -> UserId
getUserId = id

getTransactionId :: Transaction -> TransactionId
getTransactionId = id

getTransactionAmount :: Integral i => TransactionAmount -> DecimalRaw i
getTransactionAmount (TransactionAmount amount Credit) = realFracToDecimal 2 amount
getTransactionAmount (TransactionAmount amount Debit) = realFracToDecimal 2 (-amount)


data TransactionAmount = TransactionAmount
  {
    amount :: !Amount,
    transactionType :: !TransactionType
  }
  deriving ( Show, Generic)

instance Eq TransactionAmount where
  a == b = getTransactionAmount a == getTransactionAmount b
  

  