module UserService where

import UserRepository
import Models
import Database.PostgreSQL.Simple ( Connection )
import Control.Monad ( void )

getUser :: Connection -> Int -> IO (Maybe User)
getUser = getUserById

getUsers :: Connection -> IO [User]
getUsers  = getAllUsers

createUser :: Connection -> User -> IO (Maybe User)
createUser = insertUser