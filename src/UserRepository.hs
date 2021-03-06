{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UserRepository where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import Database.PostgreSQL.Simple.FromRow ()
import GHC.Int ( Int64 )
import Models
import Control.Monad ( void )

instance FromRow User

getUserAmount :: Int -> Connection -> IO (Maybe Double) 
getUserAmount userId conn = do
                amount :: [Only Double] <- query conn "SELECT amount from users where id =?"  (Only userId)
                case amount of 
                    [] -> return Nothing 
                    ((Only a):xs) -> return $ Just a 

updateUserAmount :: Int -> Double -> Connection -> IO ()
updateUserAmount userId amount conn = void (execute conn "UPDATE users set amount= ? where id=?" (amount, userId))

getAllUsers :: Connection -> IO [User]
getAllUsers conn = query_ conn "SELECT id, name, last_name, amount from users"

getUserById :: Int -> Connection -> IO (Maybe User)
getUserById userId conn = do
    rows <- query conn "SELECT id, name, last_name, amount from users where id = ?" (Only userId) 
    case rows of
        [] -> return Nothing
        (x:_) -> return $ Just x

insertUser :: User -> Connection -> IO (Maybe User)
insertUser user conn = do
    rows <- query conn "INSERT INTO users VALUES (default,?,?,0) RETURNING *" [name user, lastName user]
    case rows of
        [] -> return Nothing
        (x:_) -> return $ Just x
