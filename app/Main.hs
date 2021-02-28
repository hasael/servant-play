{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib
import Database.PostgreSQL.Simple
import Data.Pool
import Data.ByteString

main :: IO ()
main = initConnection connStr >>= startApp
        where connStr = "host=localhost port=5432 dbname=postgres user=postgres password=vehicle"

initConnection :: ByteString -> IO (Pool Connection)
initConnection connStr = createPool (connectPostgreSQL connStr) 
                                    close
                                    2 -- stripes
                                    60 --seconds to keep alive if unused
                                    10 --max 20 connection per stripe
