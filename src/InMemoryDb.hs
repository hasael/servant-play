{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE UndecidableInstances #-}
module InMemoryDb(newDB,InMemEnv(InMemEnv)) where

import Control.Concurrent.STM
import Control.Monad
import Data.Functor.Identity
import Data.Map
import Data.Maybe
import DbRepository
import GHC.IO(IO)
import GHC.Show
import Models
import System.IO.Unsafe
import Prelude (fst, print, snd, ($), (+), (++), (<$>), (==), Int, error)
import Refined
import Data.Either
import Control.Monad.IO.Class
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

data InMemEnv = InMemEnv {
  crdtState :: AppState,
  connectionsPool :: AppDatabase
}

instance HasAppState InMemEnv where
  getAppState = crdtState

instance (DbRepository IO AppDatabase) => DbRepository (MyHandler InMemEnv) InMemEnv where
  getUserAmount env userId = liftIO $ getUserAmount (connectionsPool env) userId

  updateUserAmount env userId amount = liftIO $ updateUserAmount (connectionsPool env) userId amount

  getAllUsers env = liftIO $ getAllUsers (connectionsPool env)

  getUserById env userId = liftIO $ getUserById (connectionsPool env) userId

  insertUser env user = liftIO $ insertUser (connectionsPool env) user

  getTransactionById env transactionId = liftIO $ getTransactionById (connectionsPool env) transactionId

  getTransactions env userId = liftIO $ getTransactions (connectionsPool env) userId

  getAllTransactions env = liftIO $ getAllTransactions (connectionsPool env)

  insertCreditTransaction env userId amount = liftIO $ insertCreditTransaction (connectionsPool env) userId amount

  insertDebitTransaction env userId amount = liftIO $ insertDebitTransaction (connectionsPool env) userId amount

instance (DbRepository IO AppDatabase) => DbRepository (EnvHandler InMemEnv) InMemEnv where
  getUserAmount env userId = liftIO $ getUserAmount (connectionsPool env) userId

  updateUserAmount env userId amount = liftIO $ updateUserAmount (connectionsPool env) userId amount

  getAllUsers env = liftIO $ getAllUsers (connectionsPool env)

  getUserById env userId = liftIO $ getUserById (connectionsPool env) userId

  insertUser env user = liftIO $ insertUser (connectionsPool env) user

  getTransactionById env transactionId = liftIO $ getTransactionById (connectionsPool env) transactionId

  getTransactions env userId = liftIO $ getTransactions (connectionsPool env) userId

  getAllTransactions env = liftIO $ getAllTransactions (connectionsPool env)

  insertCreditTransaction env userId amount = liftIO $ insertCreditTransaction (connectionsPool env) userId amount

  insertDebitTransaction env userId amount = liftIO $ insertDebitTransaction (connectionsPool env) userId amount


instance (DbRepository IO AppDatabase) => DbRepository IO InMemEnv where
  getUserAmount env userId = getUserAmount (connectionsPool env) userId

  updateUserAmount env userId amount = updateUserAmount (connectionsPool env) userId amount

  getAllUsers env = getAllUsers (connectionsPool env)

  getUserById env userId = getUserById (connectionsPool env) userId

  insertUser env user = insertUser (connectionsPool env) user

  getTransactionById env transactionId = getTransactionById (connectionsPool env) transactionId

  getTransactions env userId = getTransactions (connectionsPool env) userId

  getAllTransactions env = getAllTransactions (connectionsPool env)

  insertCreditTransaction env userId amount = insertCreditTransaction (connectionsPool env) userId amount

  insertDebitTransaction env userId amount = insertDebitTransaction (connectionsPool env) userId amount