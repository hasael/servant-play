{-# LANGUAGE FlexibleContexts #-}

module Aligner
  ( merge_,
    start_,
  )
where

import Control.Monad (void)
import Data.Map (Map, elems)
import DbRepository
  ( DbRepository (getAllTransactions, updateUserAmount),
  )
import GCounter
import Instances
import Models (Transaction (userId), UserId, calculatedtransactionAmount)

merge_ :: (DbRepository IO a) => a -> IO ()
merge_ conn = do
  result <- merge
  print result
  updateTrxData conn result

start_ :: (DbRepository IO a) => a -> IO ()
start_ conn = do
  trxs <- getAllTransactions conn
  sequence $ fmap (\t -> increment (userId t) t) trxs
  return ()

updateTrxData :: DbRepository IO a => a -> Map UserId Transaction -> IO ()
updateTrxData conn map = void $ sequence_ $ fmap (\t -> updateUserAmount conn (userId t) (calculatedtransactionAmount t)) $ elems map
