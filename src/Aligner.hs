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

merge_ :: (DbRepository IO a) => a -> IO ()
merge_ conn = do
  result <- merge
  print result
  updateTrxData conn result

start_ :: (DbRepository IO a) => a -> IO ()
start_ conn = do
  trxs <- getAllTransactions conn
  sequence $ fmap (\t -> increment (userId t) $ trxAmount t) trxs
  return ()

updateTrxData :: DbRepository IO a => a -> Map UserId TransactionAmount -> IO ()
--updateTrxData conn map = void $ sequence_ $ fmap (\t -> updateUserAmount conn (userId t) (fromRational $ toRational (calculatedtransactionAmount t))) $ elems map
updateTrxData conn map = sequence_ $ mapWithKey  (\k v -> updateUserAmount conn k (fromRational $ toRational (getTransactionAmount v)) ) map
