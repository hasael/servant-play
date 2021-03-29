{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DuplicateRecordFields #-}

module InMemoryDb(newDB) where

import Control.Concurrent.STM
import Control.Monad
import Data.Functor.Identity
import Data.Map
import Data.Maybe
import DbRepository
import GHC.IO
import GHC.Show
import Models
import System.IO.Unsafe
import Prelude (fst, print, snd, ($), (+), (++), (<$>), (==), Int, error)
import Refined
import Data.Either

newDB :: IO AppDatabase
newDB = do
  a <- newTVarIO empty
  b <- newTVarIO empty
  return (a, b)

type TransactionTable = TVar (Map TransactionId Transaction)

type UsersTable = TVar (Map UserId User)

type AppDatabase = (TransactionTable, UsersTable)

instance DbRepository IO AppDatabase where
  getUserAmount db userId =
    atomically $ do
      currData <- readTVar $ snd db
      let result = lookup userId currData
      return $ userAmount <$> result

  updateUserAmount db userId amount =
    atomically $ do
      currData <- readTVar $ snd db
      let userWithAmount = withAmount amount <$> lookup userId currData
      case userWithAmount of
        Just usr -> do
          let newValues = insert userId usr currData
          writeTVar (snd db) newValues
          return ()
        Nothing -> return ()

  getAllUsers db = do
    atomically $ do
      currData <- readTVar $ snd db
      let users = elems currData
      return users

  getUserById db userId =
    atomically $ do
      currData <- readTVar $ snd db
      return $ lookup userId currData

  insertUser db u =
    atomically $ do
      currData <- readTVar $ snd db
      let newId = mkUserId (size currData + 1)
      let newUser = withId u newId
      let newValues = insert newId newUser currData
      writeTVar (snd db) newValues
      return $ Just newUser

  getTransactionById db trxId =
    atomically $ do
      currData <- readTVar $ fst db
      return $ lookup trxId currData

  getAllTransactions db =
    atomically $ do
      currData <- readTVar $ fst db
      let trxs = elems currData
      return trxs

  getTransactions db usrId =
    atomically $ do
      currData <- readTVar $ fst db
      return $ elems $ filter (\t -> userId t == usrId) currData

  insertCreditTransaction db userId amount =
    atomically $ do
      currData <- readTVar $ fst db
      let newId = TransactionId (size currData + 1)
      let newTrx = Transaction newId userId amount Credit
      let newValues = insert newId newTrx currData
      writeTVar (fst db) newValues
      return $ Just newTrx

  insertDebitTransaction db userId amount =
    atomically $ do
      currData <- readTVar $ fst db
      let newId = TransactionId (size currData + 1)
      let newTrx = Transaction newId userId amount Debit
      let newValues = insert newId newTrx currData
      writeTVar (fst db) newValues
      return $ Just newTrx

mkUserId :: Int -> UserId
mkUserId id =  UserId $ fromRight (error "invalid userId") $ refine id

withId :: User -> UserId -> User
withId user userId = User userId (name user) (lastName user) ((amount :: User -> Amount) user)

withAmount ::  Amount -> User -> User
withAmount amount user = User (getUserId user) (name user) (lastName user) amount