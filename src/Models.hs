{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Models where
import GHC.Generics

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
    id       :: Int,
    userId   :: Int,
    amount   :: Double,
    transactionType :: TransactionType
  } deriving (Eq, Show, Generic)
