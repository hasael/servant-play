{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}


module Domain.Transaction where

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
import Domain.User
import Data.Maybe

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

trxAmount :: Transaction -> TransactionAmount
trxAmount (Transaction _ _ amount trxType) = TransactionAmount amount trxType

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

