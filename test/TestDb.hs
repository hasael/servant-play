{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-} 

module TestDb where

import Control.Monad
import Data.Void
import DbRepository

instance DbRepository IO () where

    getUserAmount _ userId = return $ Just 2

    updateUserAmount pool userId amount = return ()

    getAllUsers pool = return [] 

    getUserById pool userId = return Nothing

    insertUser pool u = return Nothing

    getTransactionById pool trxId = return Nothing

    getTransactions pool userId = return [] 

    insertCreditTransaction pool userId amount = return Nothing

    insertDebitTransaction pool userId amount = return Nothing 