{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Properties where

import Test.QuickCheck
import Test.QuickCheck.Monadic
import DbRepository
import Data.Maybe ( isJust )
import Models 
import Generic.Random ( genericArbitraryU )

prop_reverse :: [Int] -> Bool
prop_reverse xs = reverse (reverse xs) == xs

instance Arbitrary UserId where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary User where
  arbitrary = genericArbitraryU
  shrink = genericShrink

prop_insert_any_user :: DbRepository m a =>  a -> User -> PropertyM m ()
prop_insert_any_user conn user = do 
    res <- run $ insertUser conn user
    assert $ isJust res

prop_get_insert_user :: (DbRepository m a) =>  a -> User -> PropertyM m ()
prop_get_insert_user conn user =  do 
    ins <- run $ insertUser  conn user
    res <- case ins of
          Just usr ->  do
              get <- run $ getUserById conn $ getUserId usr
              return $ Just usr == get
          Nothing -> return False
    assert res
