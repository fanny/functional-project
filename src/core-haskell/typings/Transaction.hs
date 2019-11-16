{-# LANGUAGE DeriveGeneric #-}

module Transaction (
  Transaction(..)
) where

import GregorianCalendar
import TransactionType
import Data.Aeson
import GHC.Generics

data Transaction = Transaction {
  dateObject :: GregorianCalendar,
  textIdentifier :: String,
  value :: Integer,
  description :: String,
  docID :: String,
  transactionTypes :: [TransactionType]
} deriving (Show, Generic)

instance FromJSON Transaction
instance ToJSON Transaction