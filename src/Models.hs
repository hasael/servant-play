{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Models where
import GHC.Generics
import GHC.Base ( Eq, Double, Int, String )
import GHC.Show ( Show )
import Data.Aeson ( ToJSON, FromJSON )

instance FromJSON Transaction
instance FromJSON TransactionType
instance FromJSON User
instance ToJSON User
instance ToJSON Transaction
instance ToJSON TransactionType

data TransactionType = Debit | Credit
      deriving (Eq, Show, Generic)
      
data User = User
  { 
    id       :: Int,
    name     :: String,
    lastName :: String,
    amount   :: Double
  } deriving (Eq, Show, Generic)

data Transaction = Transaction
  { 
    id      :: Int,
    userId   :: Int,
    amount   :: Double,
    transactionType :: TransactionType
  } deriving (Eq, Show, Generic)

transactionAmount :: Transaction -> Double 
transactionAmount = amount

userAmount :: User -> Double 
userAmount = amount

getUserId :: User -> Int
getUserId = id