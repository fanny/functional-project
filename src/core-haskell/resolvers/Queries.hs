module Queries (
  getTransactionsByYear,
  getTransactionsByYearAndMonth,
  getRevenuesByYearAndMonth,
  getExpensesByYearAndMonth,
  getRevenueValue,
  getExpenseValue,
  getRemainsValue,
  getAvgRevenues,
  getAvgExpenses,
  getAvgRemains,
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

-- Retorna as transações pelo ano.
getTransactionsByYear :: Integer -> IO [Transaction]
getTransactionsByYear year = do
  transactions <- getTransactions
  return (filterByYear year transactions)

-- Retorna as transações pelo ano e mês.
getTransactionsByYearAndMonth :: Integer -> Integer -> IO [Transaction]
getTransactionsByYearAndMonth year month = do
  transactions <- getTransactions
  return (filterByYearAndMonth year month transactions)

-- Retorna as receitas e despesas pelo ano e mês.
getRevenuesAndExpenses :: Integer -> Integer -> IO [Transaction]
getRevenuesAndExpenses year month = do
  transactions <- (getTransactionsByYearAndMonth year month)
  return (filterByRevenueAndExpense transactions)

-- Retorna as receitas pelo ano.
getRevenuesByYear :: Integer -> IO [Transaction]
getRevenuesByYear year = do
  transactions <- (getTransactionsByYear year)
  return (filterByRevenue transactions)

-- Retorna as despesas pelo ano.
getExpensesByYear :: Integer -> IO [Transaction]
getExpensesByYear year = do
  transactions <- (getTransactionsByYear year)
  return (filterByExpense transactions)

-- Retorna as receitas pelo ano e mês.
getRevenuesByYearAndMonth :: Integer -> Integer -> IO [Transaction]
getRevenuesByYearAndMonth year month = do
  revenues <- (getRevenuesByYear year)
  return (filterByMonth month revenues)
  
-- Retorna as despesas pelo ano e mês.
getExpensesByYearAndMonth :: Integer -> Integer -> IO [Transaction]
getExpensesByYearAndMonth year month = do
  expenses <- (getExpensesByYear year)
  return (filterByMonth month expenses)

-- Retorna o valor total das receitas de um ano e mês.
getRevenueValue :: Integer -> Integer -> IO Double
getRevenueValue year month = do
  revenues <- (getRevenuesByYearAndMonth year month)
  return (sum (map (value) revenues))

-- Retorna o valor total das despesas de um ano e mês.
getExpenseValue :: Integer -> Integer -> IO Double
getExpenseValue year month = do
  expenses <- (getExpensesByYearAndMonth year month)
  return ((sum (map (value) expenses)) * (-1))

-- Retorna a sobra de um ano e mês.
getRemainsValue :: Integer -> Integer -> IO Double
getRemainsValue year month = do
  revenueValue <- (getRevenueValue year month)
  expenseValue <- (getExpenseValue year month)
  return (revenueValue - expenseValue)

-- Retorna a média das receitas de um ano.
getAvgRevenues :: Integer -> IO Double
getAvgRevenues year = do
  revenues <- (getRevenuesByYear year)
  return (mean (map (value) revenues))

-- Retorna a média das despesas de um ano.
getAvgExpenses :: Integer -> IO Double
getAvgExpenses year = do
  expenses <- (getExpensesByYear year)
  return ((mean (map (value) expenses)) * (-1))

-- Retorna a média das sobras de um ano.
getAvgRemains ::Integer -> IO Double
getAvgRemains year = do
  transactions <- (getTransactionsByYear year)
  return (mean (getMonthsRemains (filterByRevenueAndExpense transactions)))

-- Retorna o saldo final de um ano e mês.
getFinalBalance :: Integer -> Integer -> IO Double
getFinalBalance year month = do
  transactions <- (getTransactionsByYearAndMonth year month)
  remainer <- (getRemainsValue year month)
  return ((value (transactions !! 0)) + remainer)

-- Retorna o maior saldo de um ano e mês.
getMaxBalance :: Integer -> Integer -> IO Double
getMaxBalance year month = getMinMaxBalance' year month maximum

-- Retorna o menor saldo de um ano e mês.
getMinBalance :: Integer -> Integer -> IO Double 
getMinBalance year month = getMinMaxBalance' year month minimum

-- Retorna o maior ou menor saldo de um ano e mês.
getMinMaxBalance' :: Integer -> Integer -> ([Double] -> Double) -> IO Double 
getMinMaxBalance' year month func = do
  transactions <- (getTransactionsByYearAndMonth year month)
  revenuesAndExpenses <- (getRevenuesAndExpenses year month)
  return (func (getDaysBalances revenuesAndExpenses (getInitialBalance transactions)))

-- Retorna o fluxo de caixa de um ano e mês. 
-- O fluxo de caixa é uma lista contendo pares (dia, saldoFinalDoDia).
getCashFlow :: Integer -> Integer -> IO [(Integer, Double)]
getCashFlow year month = do
  transactions <- (getTransactionsByYearAndMonth year month)
  revenuesAndExpenses <- (getRevenuesAndExpenses year month)
  return (snd (mapAccumL (getCashFlow') (getInitialBalance transactions) (groupTransactionsByDay revenuesAndExpenses)))

-- Função auxiliar para a função getCashFlow.
getCashFlow' :: Double -> [Transaction] -> (Double, (Integer, Double))
getCashFlow' balance transactions = (newBalance, (getDay (transactions !! 0), newBalance))
  where newBalance = (getRemains transactions) + balance

  