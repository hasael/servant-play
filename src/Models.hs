{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BangPatterns #-}

module Models where
import GHC.Generics
import GHC.Base ( Eq, Double, Int, String , Ord)
import GHC.Show ( Show )
import Data.Aeson ( ToJSON, FromJSON )
import Prelude ( (+), (<))
import Database.PostgreSQL.Simple.FromField ( FromField )
import Database.PostgreSQL.Simple.ToField
import Servant

instance FromJSON Transaction
instance FromJSON TransactionType
instance FromJSON User
instance ToJSON User
instance ToJSON Transaction
instance ToJSON TransactionType
data TransactionType = Debit | Credit
      deriving (Eq, Show, Generic)

newtype UserId = UserId { u_value :: Int } deriving (Eq, Ord, Show, Generic, ToJSON, FromJSON, FromField, ToField, FromHttpApiData)
newtype TransactionId = TransactionId { t_value :: Int} deriving (Eq, Ord, Show, ToJSON, FromJSON, FromField, ToField, FromHttpApiData)
type Amount = Double 

data User = User
  { 
    id       :: !UserId,
    name     :: !String,
    lastName :: !String,
    amount   :: !Amount
  } deriving (Eq, Show, Generic)

data Transaction = Transaction
  { 
    id      :: !TransactionId,
    userId   :: !UserId,
    amount   :: !Amount,
    transactionType :: !TransactionType
  } deriving (Eq, Show, Generic)

transactionAmount :: Transaction -> Amount 
transactionAmount = amount

calculatedtransactionAmount :: Transaction -> Amount 
calculatedtransactionAmount (Transaction _ _ amount Credit) = amount
calculatedtransactionAmount (Transaction _ _ amount Debit) = -amount

userAmount :: User -> Amount 
userAmount = amount

getUserId :: User -> UserId
getUserId = id

getTransactionId :: Transaction -> TransactionId
getTransactionId = id