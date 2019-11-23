module Queries (
  getTransactionsByYear,
  getTransactionsByYearAndMonth,
  getRevenuesByYearAndMonth,
  getExpensesByYearAndMonth,
  getRevenueValueByYearAndMonth,
  getExpenseValueByYearAndMonth,
  getRemainsValueByYearAndMonth,
  getAvgRevenuesByYear,
  getAvgExpensesByYear,
  groupTransactionsByDay,
  getMaxBalance,
  getMinBalance,
  getFinalBalance,
  getDaysBalances,
  getCashFlow
) where

import Filters
import Transaction
import Helpers
import JsonParser
import GregorianCalendar
import Data.List

getTransactionsByYear :: Integer -> IO [Transaction]
getTransactionsByYear year = do
  transactions <- getTransactions
  return (filterByYear year transactions)

getTransactionsByYearAndMonth :: Integer -> Integer -> IO [Transaction]
getTransactionsByYearAndMonth year month = do
  transactions <- getTransactions
  return (filterByYearAndMonth year month transactions)

getRevenuesAndExpenses :: Integer -> Integer -> IO [Transaction]
getRevenuesAndExpenses year month = do
  transactions <- (getTransactionsByYearAndMonth year month)
  return (filter (isRevenueOrExpense) transactions)

getRevenuesByYear :: Integer -> IO [Transaction]
getRevenuesByYear year = do
  transactions <- (getTransactionsByYear year )
  return (filterByRevenue transactions)

getExpensesByYear :: Integer -> IO [Transaction]
getExpensesByYear year = do
  transactions <- (getTransactionsByYear year )
  return (filterByExpense transactions)

getRevenuesByYearAndMonth :: Integer -> Integer -> IO [Transaction]
getRevenuesByYearAndMonth year month = do
  transactions <- (getTransactionsByYearAndMonth year month)
  return (filterByRevenue transactions)

getExpensesByYearAndMonth :: Integer -> Integer -> IO [Transaction]
getExpensesByYearAndMonth year month = do
  transactions <- (getTransactionsByYearAndMonth year month)
  return (filterByExpense transactions)

getRevenueValueByYearAndMonth :: Integer -> Integer -> IO Float
getRevenueValueByYearAndMonth year month = do
  transactions <- (getRevenuesByYearAndMonth year month)
  return (sum (map (value) transactions))

getExpenseValueByYearAndMonth :: Integer -> Integer -> IO Float
getExpenseValueByYearAndMonth year month = do
  transactions <- (getExpensesByYearAndMonth year month)
  return ((sum (map (value) transactions)) * (-1))

getRemainsValueByYearAndMonth :: Integer -> Integer -> IO Float
getRemainsValueByYearAndMonth year month = do
  revenueValue <- (getRevenueValueByYearAndMonth year month)
  expenseValue <- (getExpenseValueByYearAndMonth year month)
  return (revenueValue - expenseValue)

getAvgRevenuesByYear :: Integer -> IO Float
getAvgRevenuesByYear year = do
  revenues <- (getRevenuesByYear year)
  return (mean (map (value) revenues))

getAvgExpensesByYear :: Integer -> IO Float
getAvgExpensesByYear year = do
  expenses <- (getExpensesByYear year)
  return ((mean (map (value) expenses)) * (-1))

getAvgRemainsByYear ::Integer -> IO Float
getAvgRemainsByYear year = do
  transactions <- (getTransactionsByYear year)
  return (mean (getMonthsRemains (filter isRevenueOrExpense transactions)))

-- Retorna o saldo final para um dado ano e mês.
getFinalBalance :: Integer -> Integer -> IO Float
getFinalBalance year month = do
  transactions <- (getTransactionsByYearAndMonth year month)
  remainer <- (getRemainsValueByYearAndMonth year month)
  return ((value (transactions !! 0)) + remainer)

-- Retorna o maior saldo para um dado ano e mês.
getMaxBalance :: Integer -> Integer -> IO Float
getMaxBalance year month = do
  transactions <- (getTransactionsByYearAndMonth year month)
  revenuesAndExpenses <- (getRevenuesAndExpenses year month)
  return (maximum (getDaysBalances revenuesAndExpenses (getInitialBalance transactions)))

-- Retorna o menor saldo para um dado ano e mês.
getMinBalance :: Integer -> Integer -> IO Float 
getMinBalance year month = do
  transactions <- (getTransactionsByYearAndMonth year month)
  revenuesAndExpenses <- (getRevenuesAndExpenses year month)
  return (minimum (getDaysBalances revenuesAndExpenses (getInitialBalance transactions)))

-- Retorna o fluxo de caixa de determinado mês e ano. 
-- O fluxo de caixa é do uma lista contendo pares (dia, saldoFinalDoDia). 
getCashFlow :: Integer -> Integer -> IO [(Integer, Float)]
getCashFlow year month = do
  revenuesAndExpenses <- (getRevenuesAndExpenses year month)
  transactions <- (getTransactionsByYearAndMonth year month)
  return (snd (mapAccumL (\x y -> (((getRemains y) + x), (getDay (y!!0), (getRemains y) + x))) (getInitialBalance transactions) (groupTransactionsByDay revenuesAndExpenses)))

  