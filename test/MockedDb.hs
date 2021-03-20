{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}

module MockedDb where

import Control.Concurrent.STM
import Control.Monad
import Data.Functor.Identity
import Data.Map
import Data.Maybe
import DbRepository
import GHC.IO
import Models
import System.IO.Unsafe
import Test.QuickCheck
import Test.QuickCheck.Monadic
import TestBase
import Prelude (($), (<$>), (==), (+))

instance CanPropertyTest Identity where
  toProperty = runIdentity

monadicPropId :: (CanPropertyTest IO) => PropertyM Identity () -> Property
monadicPropId = monadic toProperty

{-# NOINLINE userData #-}
userData :: TVar (Map UserId User)
userData = unsafePerformIO $ newTVarIO empty

{-# NOINLINE transactionData #-}
transactionData :: TVar (Map TransactionId Transaction )
transactionData = unsafePerformIO $ newTVarIO empty

instance DbRepository IO () where
  getUserAmount _ userId =
    atomically $ do
      currData <- readTVar userData
      let result = lookup userId currData
      return $ userAmount <$> result

  updateUserAmount pool userId amount =
    atomically $ do
      currData <- readTVar userData
      let userWithAmount = withAmount amount <$> lookup userId currData
      case userWithAmount of
        Just usr -> do
          let newValues = insert userId usr currData
          writeTVar userData newValues
          return ()
        Nothing -> return ()

  getAllUsers _ = atomically $ do
    currData <- readTVar userData
    let users = elems currData
    return users

  getUserById _ userId = atomically $ do
    currData <- readTVar userData
    return $ lookup userId currData

  insertUser _ u = atomically $ do
    currData <- readTVar userData
    let newId = UserId (size currData + 1)
    let newUser = withId u newId
    let newValues = insert newId newUser currData
    writeTVar userData newValues
    return $ Just newUser

  getTransactionById _ trxId = atomically $ do
    currData <- readTVar transactionData
    return $ lookup trxId currData

  getAllTransactions pool = atomically $ do
    currData <- readTVar transactionData
    let trxs = elems currData
    return trxs

  getTransactions pool usrId =  atomically $ do
    currData <- readTVar transactionData
    return $ elems $ filter (\t -> userId t == usrId) currData

  insertCreditTransaction _ userId amount = atomically $ do
    currData <- readTVar transactionData
    let newId = TransactionId (size currData + 1)
    let newTrx = Transaction newId userId amount Credit
    let newValues = insert newId newTrx currData
    writeTVar transactionData newValues
    return $ Just newTrx

  insertDebitTransaction _ userId amount = atomically $ do
    currData <- readTVar transactionData
    let newId = TransactionId (size currData + 1)
    let newTrx = Transaction newId userId amount Debit 
    let newValues = insert newId newTrx currData
    writeTVar transactionData newValues
    return $ Just newTrx
