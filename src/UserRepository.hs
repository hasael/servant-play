{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module UserRepository where

import Database.PostgreSQL.Simple
import Database.PostgreSQL.Simple.FromField (FromField, fromField)
import Database.PostgreSQL.Simple.FromRow ()
import GHC.Int ( Int64 )
import Models

instance FromRow User

getUserAmount :: Connection -> Int -> IO (Maybe Double) 
getUserAmount conn userId = do
                amount :: [Only Double] <- query conn "SELECT amount from users where id =?"  (Only userId)
                case amount of 
                    [] -> return Nothing 
                    ((Only a):xs) -> return $ Just a 

updateUserAmount :: Connection -> Int -> Double -> IO Int64
updateUserAmount conn userId amount = execute conn "UPDATE users set amount= ? where id=?" (amount, userId)

getAllUsers :: Connection -> IO [User]
getAllUsers conn = query_ conn "SELECT id, name, last_name, amount from users"

getUserById :: Connection -> Int -> IO (Maybe User)
getUserById conn userId = do
    rows <- query conn "SELECT id, name, last_name, amount from users where id = ?" (Only userId) 
    case rows of
        [] -> return Nothing
        (x:_) -> return $ Just x

insertUser :: Connection -> User -> IO (Maybe User)
insertUser conn user = do
    rows <- query conn "INSERT INTO users VALUES (default,?,?,0) RETURNING *" [name user, lastName user]
    case rows of
        [] -> return Nothing
        (x:_) -> return $ Just x
