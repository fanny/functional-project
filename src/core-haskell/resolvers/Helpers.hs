module Helpers ( 
  isRevenue,
  isExpense,
  isRevenueOrExpense,
  checkYear,
  checkMonth,
  mean,
  getDay,
  groupTransactionsByDay,
  getDaysRemains,
  getRemains,
  getDaysBalances,
  getInitialBalance,
  groupTransactionsByMonth,
  getMonthsRemains
) where

import GregorianCalendar
import Transaction
import TransactionType
import Data.List
import Data.Function

-- Checa o ano de uma transação.
checkYear :: Integer -> Transaction -> Bool
checkYear y transaction = year (date transaction) == y

-- Checa o mês de uma transação.
checkMonth :: Integer -> Transaction -> Bool
checkMonth m transaction = month (date transaction) == m

-- Checa se uma transação é uma receita ou despesa.
isRevenueOrExpense :: Transaction -> Bool
isRevenueOrExpense transaction = ([VALOR_APLICACAO, APLICACAO, SALDO_CORRENTE] `intersect` (transactionTypes transaction)) == []

-- Checa se uma transação é uma receita.
isRevenue :: Transaction -> Bool
isRevenue transaction = (value transaction) >= 0 && (isRevenueOrExpense transaction)

-- Checa se uma transação é uma despesa.
isExpense :: Transaction -> Bool
isExpense transaction = (value transaction) < 0 && (isRevenueOrExpense transaction)

-- Calcula a média para uma lista de valores.
mean :: [Double] -> Double
mean values = sum (values) / fromIntegral (length values)

-- Agrupa uma lista de transações.
groupTransactions :: [Transaction] -> (Transaction -> Integer) -> [[Transaction]]
groupTransactions transactions func = (groupBy ((==) `on` func) transactions)

-- Agrupa uma lista de transações por dia.
groupTransactionsByDay :: [Transaction] -> [[Transaction]]
groupTransactionsByDay transactions = groupTransactions transactions getDay

-- Agrupa uma lista de transações por mês.
groupTransactionsByMonth :: [Transaction] -> [[Transaction]]
groupTransactionsByMonth transactions = groupTransactions transactions getMonth

-- Retorna uma lista contendo a sobra para cada dia da lista de transações.
getDaysRemains :: [Transaction] -> [Double]
getDaysRemains transactions = map (getRemains) (groupTransactionsByDay transactions)

-- Retorna uma lista contendo a sobra para cada dia da lista de transações.
getMonthsRemains :: [Transaction] -> [Double]
getMonthsRemains transactions = map (getRemains) (groupTransactionsByMonth transactions)

-- Retorna a sobra para a lista de transações.
getRemains :: [Transaction] -> Double
getRemains transactions = sum (map (value) transactions)

-- Retorna uma lista contendo o saldo para cada dia da lista de transações.
getDaysBalances :: [Transaction] -> Double -> [Double]
getDaysBalances revenuesAndExpenses initialBalance = 
  snd (mapAccumL (\x y -> (x+y,x+y)) initialBalance (getDaysRemains revenuesAndExpenses))

-- Retorna o saldo inicial de uma lista de transaçãoes.
getInitialBalance :: [Transaction] -> Double
getInitialBalance transactions = value (transactions !! 0)

-- Retorna o dia do mês de uma transação.
getDay :: Transaction -> Integer
getDay t = dayOfMonth (date t)

-- Retorna o mês de uma transação.
getMonth :: Transaction -> Integer
getMonth t = month (date t)