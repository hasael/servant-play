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

merge_ :: (DbRepository IO a) => a -> AppState -> IO ()
merge_ conn state = do
  result <- merge state
  print result
  updateTrxData conn result

start_ :: (DbRepository IO a) => a -> AppState -> IO ()
start_ conn state = do
  trxs <- getAllTransactions conn
  sequence $ fmap (\t -> increment state (userId t) $ trxAmount t) trxs
  return ()

updateTrxData :: DbRepository IO a => a -> Map UserId TransactionAmount -> IO ()
updateTrxData conn map = sequence_ $ mapWithKey  (\k v -> updateUserAmount conn k (fromRational $ toRational (calculatedTransactionAmount v)) ) map
