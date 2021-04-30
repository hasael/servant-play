{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ImplicitParams #-}

module Domain.DbRepository where

import Domain.User
import Domain.Transaction
import Domain.Helper
import Control.Monad.Reader

class Monad m => DbRepository m a where

  getUserAmount :: a -> UserId -> m (Maybe Amount)

  updateUserAmount :: a -> UserId -> Amount -> m ()

  getAllUsers :: a -> m [User]

  getUserById :: a -> UserId -> m (Maybe User)

  insertUser :: a -> User -> m (Maybe User)

  getTransactionById :: a -> TransactionId -> m (Maybe Transaction)

  getTransactions :: a -> UserId -> m [Transaction]

  getAllTransactions :: a -> m [Transaction]

  insertCreditTransaction :: a -> UserId -> Amount -> m (Maybe Transaction)

  insertDebitTransaction :: a -> UserId -> Amount -> m (Maybe Transaction)



