module JsonParser (
  getTransactions,
  getTestTransactions
) where

import qualified Data.ByteString.Lazy as B
import Data.Aeson
import Transaction
import Data.Maybe

-- Tranforma o texto passado para um caminho de arquivo.
getJsonFilePath :: String -> FilePath
getJsonFilePath s = s

-- Retorna o arquivo do caminho passado.
getFile :: String -> IO B.ByteString
getFile s = B.readFile (getJsonFilePath s)

-- Retorna a lista de transações contidas no arquivo de dados passado.
getTransactions' :: String -> IO [Transaction]
getTransactions' fileName = do
    transactions <- (decode <$> (getFile fileName)) :: IO (Maybe [Transaction])
    return (fromJust transactions)

-- Retorna a lista de transações contidas no arquivo de dados original.
getTransactions :: IO [Transaction]
getTransactions = getTransactions' "../../data/transactions.json"

-- Retorna a lista de transações contidas no arquivo de dados de testes.
getTestTransactions :: IO [Transaction]
getTestTransactions = getTransactions' "../../data/testTransactions.json"