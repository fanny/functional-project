module JsonParser (
  getTransactions
) where

import qualified Data.ByteString.Lazy as B
import Data.Aeson
import Transaction
import Data.Maybe

getJsonFilePath :: FilePath
getJsonFilePath = "../../data/transactions.json"

getFile :: IO B.ByteString
getFile = B.readFile getJsonFilePath

getTransactions :: IO [Transaction]
getTransactions = do
    transactions <- (decode <$> getFile) :: IO (Maybe [Transaction])
    return (fromJust transactions)