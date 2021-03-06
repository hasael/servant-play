module TransactionService where

import UserRepository
import TransactionRepository
import Models
import Database.PostgreSQL.Simple ( Connection )
import Control.Monad ( void )

data DebitOpResult = CorrectDebit Transaction | DebitUserNotFound | IncorrectAmount
data CreditOpResult = CorrectCredit Transaction | CreditUserNotFound

createDebitTransaction :: Connection -> Int -> Double -> IO DebitOpResult
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

createCreditTransaction :: Connection -> Int -> Double -> IO CreditOpResult
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

getUserTransactions :: Connection -> Int -> IO [Transaction]
getUserTransactions = getTransactions 

getTransaction :: Connection -> Int -> IO (Maybe Transaction)
getTransaction  =  getTransactionById