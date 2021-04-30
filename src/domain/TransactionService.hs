module Domain.TransactionService where

import Control.Monad (void)
import Database.PostgreSQL.Simple (Connection)
import Domain.DbRepository
import Domain.Models

data DebitOpResult = CorrectDebit Transaction | DebitUserNotFound | IncorrectAmount

data CreditOpResult = CorrectCredit Transaction | CreditUserNotFound

createDebitTransaction :: DbRepository m a => a -> UserId -> Amount  -> m DebitOpResult
createDebitTransaction conn userId amount = do
  curramount <- getUserAmount conn userId
  let newAmount = (\a -> a - amount) <$> curramount
  case newAmount of
    Just a ->
      if a >= 0
        then do
          trx <- insertDebitTransaction conn userId amount
          case trx of
            Just t -> updateUserAmount conn userId a >> return (CorrectDebit t)
            Nothing -> return DebitUserNotFound
        else return IncorrectAmount
    Nothing -> return DebitUserNotFound

createCreditTransaction :: DbRepository m a => a -> UserId -> Amount  -> m CreditOpResult
createCreditTransaction conn userId amount = do
  curramount <- getUserAmount conn userId
  let newAmount = (+ amount) <$> curramount
  case newAmount of
    Just a -> do
      trx <- insertCreditTransaction conn userId amount
      case trx of
        Just t -> updateUserAmount conn userId a >> return (CorrectCredit t)
        Nothing -> return CreditUserNotFound
    Nothing -> return CreditUserNotFound
