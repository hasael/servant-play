{-# LANGUAGE FlexibleContexts#-} 

module Aligner
(
    merge_,
    start_
)
 where

import Models
import DbRepository
import GCounter
import Control.Monad
import Data.Map

import Instances

merge_ :: (DbRepository IO a) => a -> IO ()
merge_ conn = do 
              result <- merge
              updateTrxData conn result

start_ :: (DbRepository IO a)  => a -> IO ()
start_ conn = do
     trxs <- getAllTransactions conn
     sequence $ fmap (\t -> increment (userId t) t ) trxs
     return ()


updateTrxData :: DbRepository IO a => a -> Map Int Transaction -> IO ()
updateTrxData conn map = void $ sequence_ $ fmap (\t -> updateUserAmount conn (userId t) (calculatedtransactionAmount t)) $ elems map