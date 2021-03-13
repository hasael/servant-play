{-# LANGUAGE FlexibleContexts#-} 

module Aligner
(
    merge_,
    start_
)
 where

import Models ( Transaction(userId), calculatedtransactionAmount )
import DbRepository
    ( DbRepository(updateUserAmount, getAllTransactions) )
import GCounter
import Control.Monad ( void )
import Data.Map ( elems, Map )

import Instances

merge_ :: (DbRepository IO a) => a -> IO ()
merge_ conn = do 
              result <- merge
              print result
              updateTrxData conn result

start_ :: (DbRepository IO a)  => a -> IO ()
start_ conn = do
     trxs <- getAllTransactions conn
     sequence $ fmap (\t -> increment (userId t) t ) trxs
     return ()


updateTrxData :: DbRepository IO a => a -> Map Int Transaction -> IO ()
updateTrxData conn map = void $ sequence_ $ fmap (\t -> updateUserAmount conn (userId t) (calculatedtransactionAmount t)) $ elems map