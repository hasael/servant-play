
{-# LANGUAGE MultiParamTypeClasses #-}

module Instances where

import Models
import GCounter

instance Semigroup Transaction where
  a <> b = Transaction 0 (userId a) finalAmount trxType
            where finalAmount = calculatedtransactionAmount a + calculatedtransactionAmount b
                  trxType = if finalAmount < 0 then Debit else Credit

instance Monoid Transaction where
  mempty = Transaction 0 0 0 Credit

instance GCounter Transaction Int 