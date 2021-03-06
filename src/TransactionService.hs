module TransactionService where

import Models
import Database.PostgreSQL.Simple ( Connection )
import Control.Monad ( void )
import DbRepository

data DebitOpResult = CorrectDebit Transaction | DebitUserNotFound | IncorrectAmount
data CreditOpResult = CorrectCredit Transaction | CreditUserNotFound

createDebitTransaction :: DbRepository m a => a -> Int -> Double -> m DebitOpResult
createDebitTransaction  conn userId amount =  do                       
                          curramount <- getUserAmount conn userId
                          let newAmount = (\a -> a - amount) <$> curramount
                          case newAmount of 
                            Just a -> if a>=0 then do
                                         trx <- insertDebitTransaction conn userId amount
                                         case trx of
                                             Just t -> updateUserAmount conn userId a >> return (CorrectDebit t)
                                             Nothing -> return DebitUserNotFound
                                      else
                                        return IncorrectAmount
                            Nothing -> return DebitUserNotFound

createCreditTransaction :: DbRepository m a => a -> Int -> Double -> m CreditOpResult
createCreditTransaction  conn userId amount =  do                       
                          curramount <- getUserAmount conn userId
                          let newAmount = (+ amount) <$> curramount
                          case newAmount of 
                            Just a ->  do
                                         trx <- insertCreditTransaction conn userId amount
                                         case trx of
                                             Just t -> updateUserAmount conn userId a >> return (CorrectCredit t)
                                             Nothing -> return CreditUserNotFound
                            Nothing -> return CreditUserNotFound

getUserTransactions :: DbRepository m a => a  -> Int -> m [Transaction]
getUserTransactions = getTransactions 

getTransaction :: DbRepository m a => a  -> Int -> m (Maybe Transaction)
getTransaction  =  getTransactionById