{-# LANGUAGE FlexibleContexts #-}

module Aligner
  ( merge_,
    start_,
  )
where

import Control.Monad (void)
import Control.Monad.Reader
import Data.Map
import Domain.AppState
import Domain.DbRepository
  ( DbRepository (getAllTransactions, updateUserAmount),
  )
import Domain.GCounter
import Domain.Transaction
import Domain.User

merge_ :: (DbRepository m env, GCounter TransactionAmount UserId, MonadReader env m, HasAppState env, MonadIO m) => m ()
merge_ = do
  env <- ask
  let state = getAppState env
  result <- liftIO $ merge state
  liftIO $ print $ "Merge result: " ++ show result
  updateTrxData env result

start_ :: (DbRepository m env, GCounter TransactionAmount UserId, MonadReader env m, HasAppState env, MonadIO m) => m ()
start_ = do
  env <- ask
  let state = getAppState env
  trxs <- getAllTransactions env
  void $ liftIO $ sequence $ fmap (\t -> increment state (userId t) $ trxAmount t) trxs

updateTrxData :: (DbRepository m env, MonadReader env m, HasAppState env) => env -> Map UserId TransactionAmount -> m ()
updateTrxData env map = sequence_ $ mapWithKey (\k v -> updateUserAmount env k (fromRational $ toRational (calculatedTransactionAmount v))) map
