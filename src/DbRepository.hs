{-# LANGUAGE MultiParamTypeClasses      #-}

module DbRepository where

import Models

class Monad m => DbRepository m a where 


    getUserAmount :: a -> Int -> m (Maybe Double)

    updateUserAmount :: a -> Int -> Double -> m ()

    getAllUsers :: a -> m [User]

    getUserById :: a -> Int -> m (Maybe User)

    insertUser :: a -> User -> m (Maybe User)

    getTransactionById :: a -> Int -> m (Maybe Transaction)

    getTransactions :: a -> Int -> m [Transaction]

    getAllTransactions :: a -> m [Transaction]

    insertCreditTransaction :: a -> Int -> Double -> m (Maybe Transaction) 

    insertDebitTransaction :: a -> Int -> Double -> m (Maybe Transaction)


