{-# LANGUAGE MultiParamTypeClasses #-}

module Instances where

import GCounter
import Models
import Data.Ratio
import Data.Decimal

instance Semigroup TransactionAmount where
  a <> b = TransactionAmount finalAmount trxType
    where
      calcAmount = fromRational $ toRational ( calculatedTransactionAmount a + calculatedTransactionAmount b) 
      finalAmount = abs calcAmount
      trxType = if calcAmount < 0 then Debit else Credit

instance Monoid TransactionAmount where
  mempty = TransactionAmount 0.0 Credit

instance GCounter TransactionAmount UserId
