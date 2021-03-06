module UserService where

import Models
import Database.PostgreSQL.Simple ( Connection )
import Control.Monad ( void )
import DbRepository

getUser :: DbRepository m a=> a ->Int -> m (Maybe User)
getUser = getUserById

getUsers :: DbRepository m a => a -> m [User]
getUsers  = getAllUsers

createUser :: DbRepository m a => a -> User -> m (Maybe User)
createUser = insertUser