{-# LANGUAGE FlexibleContexts #-}

module Aligner
  ( merge_,
    start_,
  )
where

import Control.Monad (void)
import Data.Map 
import DbRepository
  ( DbRepository (getAllTransactions, updateUserAmount),
  )
import GCounter
import Instances
import Models 
import Control.Monad.Reader

merge_ :: (DbRepository m env, MonadReader env m, HasAppState env, MonadIO m) => m ()
merge_ = do
  env <- ask
  let state = getAppState env
  result <- liftIO $ merge state
  liftIO $ print $ "Merge result: " ++ show result
  updateTrxData env result

start_ :: (DbRepository m env, MonadReader env m, HasAppState env, MonadIO m) => m ()
start_ = do
  env <- ask
  let state = getAppState env
  trxs <- getAllTransactions env
  void $ liftIO $ sequence $ fmap (\t -> increment state (userId t) $ trxAmount t) trxs
  

updateTrxData :: (DbRepository m env, MonadReader env m, HasAppState env)  => env -> Map UserId TransactionAmount -> m ()
updateTrxData env map = sequence_ $ mapWithKey  (\k v -> updateUserAmount env k (fromRational $ toRational (calculatedTransactionAmount v)) ) map
