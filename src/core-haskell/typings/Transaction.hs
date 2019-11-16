{-# LANGUAGE DeriveGeneric #-}

module Transaction (
  Transaction(..)
) where

import GregorianCalendar
import TransactionType
import Data.Aeson
import GHC.Generics

data Transaction = Transaction {
  date :: GregorianCalendar,
  textIdentifier :: String,
  value :: Float,
  description :: String,
  docID :: String,
  transactionTypes :: [TransactionType]
} deriving (Show, Generic)

instance FromJSON Transaction
instance ToJSON Transaction