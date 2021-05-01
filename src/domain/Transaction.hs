{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Domain.Transaction where

import Control.Monad.Reader
import Data.Aeson (FromJSON, ToJSON)
import Data.Decimal
import Data.Either.Combinators (mapLeft)
import Data.Maybe
import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField (FromField (fromField))
import Database.PostgreSQL.Simple.ToField
import Domain.Helper
import Domain.User
import GHC.Base (Eq, Int, Ord)
import GHC.Generics
import GHC.Show
import Servant
import Prelude (Integral, (==))

instance FromJSON Transaction

instance FromJSON TransactionType

instance ToJSON Transaction

instance ToJSON TransactionType

instance Eq TransactionAmount where
  a == b = calculatedTransactionAmount a == calculatedTransactionAmount b

instance FromField TransactionType where
  fromField f (Just "Debit") = return Debit
  fromField f _ = return Credit

instance FromRow Transaction

data TransactionType = Debit | Credit
  deriving (Eq, Show, Generic)

newtype TransactionId = TransactionId {t_value :: Int}
  deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, FromField, ToField, FromHttpApiData)

data Transaction = Transaction
  { id :: !TransactionId,
    userId :: !UserId,
    amount :: !Amount,
    transactionType :: !TransactionType
  }
  deriving (Eq, Show, Generic)

data TransactionAmount = TransactionAmount
  { amount :: !Amount,
    transactionType :: !TransactionType
  }
  deriving (Show, Generic)

trxAmount :: Transaction -> TransactionAmount
trxAmount (Transaction _ _ amount trxType) = TransactionAmount amount trxType

getTransactionId :: Transaction -> TransactionId
getTransactionId = id

calculatedTransactionAmount :: Integral i => TransactionAmount -> DecimalRaw i
calculatedTransactionAmount (TransactionAmount amount Credit) = realFracToDecimal 2 amount
calculatedTransactionAmount (TransactionAmount amount Debit) = realFracToDecimal 2 (- amount)
