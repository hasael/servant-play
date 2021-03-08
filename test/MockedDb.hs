{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-} 
{-# LANGUAGE FlexibleContexts           #-} 

module MockedDb where

import Control.Monad
import DbRepository
import Data.Functor.Identity
import TestBase
import Test.QuickCheck
import Test.QuickCheck.Monadic

instance CanPropertyTest Identity where
    toProperty = runIdentity


monadicPropId :: (CanPropertyTest IO) => PropertyM Identity () -> Property
monadicPropId = monadic toProperty 

instance DbRepository Identity () where

    getUserAmount _ userId = return $ Just 2

    updateUserAmount pool userId amount = return ()

    getAllUsers pool = return [] 

    getUserById pool userId = return Nothing

    insertUser pool u = return Nothing

    getTransactionById pool trxId = return Nothing

    getTransactions pool userId = return [] 

    insertCreditTransaction pool userId amount = return Nothing

    insertDebitTransaction pool userId amount = return Nothing 