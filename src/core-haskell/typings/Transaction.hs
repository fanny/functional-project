module Transaction (
  Transaction(..)
) where

import GregorianCalendar
import TransactionType

data Transaction = Transaction {
  dateObject :: GregorianCalendar,
  textIdentifier :: String,
  value :: Integer,
  description :: String,
  docID :: String,
  transactionTypes :: [TransactionType]
} deriving (Show);