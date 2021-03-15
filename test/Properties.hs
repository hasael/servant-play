{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeOperators #-}

module Properties where

import Data.Maybe (isJust)
import DbRepository
import Generic.Random (genericArbitraryU)
import Models
import Test.QuickCheck
import Test.QuickCheck.Monadic

prop_reverse :: [Int] -> Bool
prop_reverse xs = reverse (reverse xs) == xs

instance Arbitrary UserId where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary User where
  arbitrary = genericArbitraryU
  shrink = genericShrink

prop_insert_any_user :: DbRepository m a => a -> User -> PropertyM m ()
prop_insert_any_user conn user = do
  res <- run $ insertUser conn user
  assert $ isJust res

prop_get_insert_user :: (DbRepository m a) => a -> User -> PropertyM m ()
prop_get_insert_user conn user = do
  ins <- run $ insertUser conn user
  res <- case ins of
    Just usr -> do
      get <- run $ getUserById conn $ getUserId usr
      return $ Just usr == get
    Nothing -> return False
  assert res
