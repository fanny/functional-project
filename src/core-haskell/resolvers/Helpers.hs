module Helpers ( 
  isRevenue,
  isExpense,
  isRevenueOrExpense,
  checkYear,
  checkMonth,
  mean
) where

import GregorianCalendar
import Transaction
import TransactionType
import Data.List

checkYear :: Integer -> Transaction -> Bool
checkYear y t = year (date t) == y

checkMonth :: Integer -> Transaction -> Bool
checkMonth m t = month (date t) == m

isRevenueOrExpense :: Transaction -> Bool
isRevenueOrExpense t = ([VALOR_APLICACAO, APLICACAO, SALDO_CORRENTE] `intersect` (transactionTypes t)) == []

isRevenue :: Transaction -> Bool
isRevenue t = (value t) >= 0 && (isRevenueOrExpense t)

isExpense :: Transaction -> Bool
isExpense t = (value t) < 0 && (isRevenueOrExpense t)

mean :: [Transaction] -> Float
mean transactions = sum (map (value) transactions) / fromIntegral (length transactions)

-- Retorna uma lista contendo o saldo para cada dia da lista de transações.
getDaysBalances :: [Transaction] -> [Float]
getDaysBalances transactions = map (getBalance) (groupTransactionsByDay transactions)

-- Retorna o saldo para a lista de transações.
getBalance :: [Transaction] -> Float
getBalance transactions = sum (map (value) transactions)

-- Agrupa a lista de transações por dia.
groupTransactionsByDay :: [Transaction] -> [[Transaction]]
groupTransactionsByDay transactions = (groupBy ((==) `on` getDay) transactions)

-- Retorna o dia do mês de uma transação.
getDay :: Transaction -> Integer
getDay t = dayOfMonth (date t)