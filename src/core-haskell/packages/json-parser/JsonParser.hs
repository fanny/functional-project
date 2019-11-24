module JsonParser (
  getTransactions
) where

import qualified Data.ByteString.Lazy as B
import Data.Aeson
import Transaction
import Data.Maybe

-- Retorna o caminho para o arquivo de dados.
getJsonFilePath :: FilePath
getJsonFilePath = "../../data/transactions.json"

-- Retorna o arquivo dos dados.
getFile :: IO B.ByteString
getFile = B.readFile getJsonFilePath

-- Retorna a lista de transações contidas no arquivo dos dados.
getTransactions :: IO [Transaction]
getTransactions = do
    transactions <- (decode <$> getFile) :: IO (Maybe [Transaction])
    return (fromJust transactions)