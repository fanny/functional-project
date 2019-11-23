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

mean :: [Double] -> Double
mean values = sum (values) / fromIntegral (length values)

-- Agrupa a lista de transações por dia.
groupTransactionsByDay :: [Transaction] -> [[Transaction]]
groupTransactionsByDay transactions = (groupBy ((==) `on` getDay) transactions)

-- Agrupa a lista de transações por mês.
groupTransactionsByMonth :: [Transaction] -> [[Transaction]]
groupTransactionsByMonth transactions = (groupBy ((==) `on` getMonth) transactions)

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

-- Retorn o mês de uma transação
getMonth :: Transaction -> Integer
getMonth t = month (date t)