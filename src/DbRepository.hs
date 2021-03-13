{-# LANGUAGE MultiParamTypeClasses      #-}

module DbRepository where

import Models ( Transaction, User, UserId, TransactionId, Amount )

class Monad m => DbRepository m a where 

    getUserAmount :: a -> UserId  -> m (Maybe Double)

    updateUserAmount :: a -> UserId -> Double -> m ()

    getAllUsers :: a -> m [User]

    getUserById :: a -> UserId -> m (Maybe User)

    insertUser :: a -> User -> m (Maybe User)

    getTransactionById :: a -> TransactionId -> m (Maybe Transaction)

    getTransactions :: a -> UserId -> m [Transaction]

    getAllTransactions :: a -> m [Transaction]

    insertCreditTransaction :: a -> UserId -> Amount  -> m (Maybe Transaction) 

    insertDebitTransaction :: a -> UserId -> Amount  -> m (Maybe Transaction)


