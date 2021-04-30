{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module DBProperties where

import Data.Maybe (fromJust, fromMaybe, isJust)
import Domain.DbRepository
import GHC.Generics
import Generic.Random
import Domain.Models
import Test.QuickCheck
import Test.QuickCheck.Monadic
import TestBase

import Data.Either 
import Refined as R


instance Arbitrary UserId where
  arbitrary = do
    --as <- listOf (arbitrary :: Gen Int)
    b <- choose (1,1000000) 
    case refine b of
      Right a -> return $ UserId $ a
      Left _ -> error "error generating user id arbitrary"
    
  shrink (UserId v) = UserId <$> shrink v

instance Arbitrary User where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary TransactionAmount where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary Transaction where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary TransactionId where
  arbitrary = genericArbitraryU
  shrink = genericShrink

instance Arbitrary TransactionType where
  arbitrary = genericArbitraryU
  shrink = genericShrink

prop_insert_any_user :: DbRepository m a => a -> User -> PropertyM m ()
prop_insert_any_user conn user = do
  res <- run $ insertUser conn user
  assert $ isJust res

prop_user_read_after_insert :: (DbRepository m a) => a -> User -> PropertyM m ()
prop_user_read_after_insert conn user = do
  ins <- run $ insertUser conn user
  res <- case ins of
    Just usr -> do
      get <- run $ getUserById conn $ getUserId usr
      return $ Just usr == get
    Nothing -> return False
  assert res

prop_user_idempotent_read :: (DbRepository m a) => a -> User -> PropertyM m ()
prop_user_idempotent_read conn user = do
  ins <- run $ insertUser conn user
  res <- case ins of
    Just usr -> do
      get1 <- run $ getUserById conn $ getUserId usr
      get2 <- run $ getUserById conn $ getUserId usr
      return $ get1 == get2 && isJust get1
    Nothing -> return False
  assert res

prop_user_correct_amount_after_update :: (DbRepository IO a) => a -> UserId -> Amount -> PropertyM IO ()
prop_user_correct_amount_after_update conn usrId amount = do
  ins <- run $ insertUser conn $ defaultUser usrId
  let newUserId = getUserId $ fromJust ins
  _ <- run $ updateUserAmount conn newUserId amount
  get <- run $ getUserById conn newUserId
  run $ print $ show ins ++ "---" ++ show get ++ "--" ++ show amount
  assert $ maybe False (\u -> userAmount u == amount) get

prop_transaction_insert_any :: DbRepository m a => a -> UserId -> Amount -> PropertyM m ()
prop_transaction_insert_any conn usrId amount = do
  res <- run $ insertCreditTransaction conn usrId amount
  assert $ isJust res

prop_transaction_read_after_insert :: (DbRepository m a) => a -> UserId -> Amount -> PropertyM m ()
prop_transaction_read_after_insert conn usrId amount = do
  ins <- run $ insertCreditTransaction conn usrId amount
  res <- case ins of
    Just trx -> do
      get <- run $ getTransactionById conn $ getTransactionId trx
      trxs <- run $ getTransactions conn usrId
      return $ Just trx == get && elem trx trxs
    Nothing -> return False
  assert res

prop_transaction_idempotent_read :: (DbRepository m a) => a -> UserId -> Amount -> PropertyM m ()
prop_transaction_idempotent_read conn usrId amount = do
  ins <- run $ insertCreditTransaction conn usrId amount
  res <- case ins of
    Just trx -> do
      get1 <- run $ getTransactionById conn $ getTransactionId trx
      trxs1 <- run $ getTransactions conn usrId
      get2 <- run $ getTransactionById conn $ getTransactionId trx
      trxs2 <- run $ getTransactions conn usrId
      return $ get2 == get1 && trxs1 == trxs2
    Nothing -> return False
  assert res
