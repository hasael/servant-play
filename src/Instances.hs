{-# LANGUAGE MultiParamTypeClasses #-}

module Instances where

import GCounter
import Models

instance Semigroup Transaction where
  a <> b = Transaction (TransactionId 0) (userId a) finalAmount trxType
    where
      finalAmount = calculatedtransactionAmount a + calculatedtransactionAmount b
      trxType = if finalAmount < 0 then Debit else Credit

instance Monoid Transaction where
  mempty = Transaction (TransactionId 0) (UserId 0) 0 Credit

instance GCounter Transaction UserId
