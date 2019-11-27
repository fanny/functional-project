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
getTransactionsByYear :: [Transaction] -> Integer -> [Transaction]
getTransactionsByYear db year = filterByYear year db

-- Retorna as transações pelo ano e mês.
getTransactionsByYearAndMonth :: [Transaction] -> Integer -> Integer -> [Transaction]
getTransactionsByYearAndMonth db year month = filterByYearAndMonth year month db

-- Retorna as receitas e despesas pelo ano e mês.
getRevenuesAndExpenses :: [Transaction] -> Integer -> Integer -> [Transaction]
getRevenuesAndExpenses db year month = filterByRevenueAndExpense transactions
  where transactions = getTransactionsByYearAndMonth db year month

-- Retorna as receitas pelo ano.
getRevenuesByYear :: [Transaction] -> Integer -> [Transaction]
getRevenuesByYear db year = filterByRevenue transactions
  where transactions = getTransactionsByYear db year

-- Retorna as despesas pelo ano.
getExpensesByYear :: [Transaction] -> Integer -> [Transaction]
getExpensesByYear db year = filterByExpense transactions
  where transactions = getTransactionsByYear db year

-- Retorna as receitas pelo ano e mês.
getRevenuesByYearAndMonth :: [Transaction] -> Integer -> Integer -> [Transaction]
getRevenuesByYearAndMonth db year month = filterByMonth month revenues
  where revenues = getRevenuesByYear db year
  
-- Retorna as despesas pelo ano e mês.
getExpensesByYearAndMonth :: [Transaction] -> Integer -> Integer -> [Transaction]
getExpensesByYearAndMonth db year month = filterByMonth month expenses
  where expenses = getExpensesByYear db year

-- Retorna o valor total das receitas de um ano e mês.
getRevenueValue :: [Transaction] -> Integer -> Integer -> Double
getRevenueValue db year month = sum (map (value) revenues)
  where revenues = getRevenuesByYearAndMonth db year month

-- Retorna o valor total das despesas de um ano e mês.
getExpenseValue :: [Transaction] -> Integer -> Integer -> Double
getExpenseValue db year month = abs(sum (map (value) expenses))
  where expenses = getExpensesByYearAndMonth db year month

-- Retorna a sobra de um ano e mês.
getRemainsValue :: [Transaction] -> Integer -> Integer -> Double
getRemainsValue db year month = revenueValue - expenseValue
  where
    revenueValue = getRevenueValue db year month
    expenseValue = getExpenseValue db year month
  
-- Retorna a média das receitas de um ano.
getAvgRevenues :: [Transaction] -> Integer -> Double
getAvgRevenues db year = mean (map (value) revenues)
  where revenues = getRevenuesByYear db year

-- Retorna a média das despesas de um ano.
getAvgExpenses :: [Transaction] -> Integer -> Double
getAvgExpenses db year = abs(mean (map (value) expenses))
  where expenses = getExpensesByYear db year

-- Retorna a média das sobras de um ano.
getAvgRemains :: [Transaction] -> Integer -> Double
getAvgRemains db year = mean (getMonthsRemains (filterByRevenueAndExpense transactions))
  where transactions = getTransactionsByYear db year
  
-- Retorna o saldo final de um ano e mês.
getFinalBalance :: [Transaction] -> Integer -> Integer -> Double
getFinalBalance db year month = (getInitialBalance transactions) + remainer
  where
    transactions = getTransactionsByYearAndMonth db year month
    remainer = getRemainsValue db year month

-- Retorna o saldo mínimo de um ano e mês.
getMinBalance :: [Transaction] -> Integer -> Integer -> Double
getMinBalance db year month = getMinMaxBalance' db year month minimum

-- Retorna o saldo máximo de um ano e mês.
getMaxBalance :: [Transaction] -> Integer -> Integer -> Double
getMaxBalance db year month = getMinMaxBalance' db year month maximum
  
-- Função auxiliar para cálculo do saldo máximo e mínimo de um ano e mês.
getMinMaxBalance' :: [Transaction] -> Integer -> Integer -> ([Double] -> Double) -> Double
getMinMaxBalance' db year month func = func (snd (mapAccumL (\x y -> (x + (value y), x + (value y))) 0 revenuesAndExpenses))
  where
    transactions = getTransactionsByYearAndMonth db year month
    revenuesAndExpenses =  [transactions !! 0] ++ (getRevenuesAndExpenses db year month)

-- Retorna o fluxo de caixa de um ano e mês. 
-- O fluxo de caixa é uma lista contendo pares (dia, saldoFinalDoDia).
getCashFlow :: [Transaction] -> Integer -> Integer -> [(Integer, Double)]
getCashFlow db year month = snd (mapAccumL (getCashFlow') 0 (groupTransactionsByDay revenuesAndExpenses))
  where
    transactions = getTransactionsByYearAndMonth db year month
    revenuesAndExpenses =  [transactions !! 0] ++ (getRevenuesAndExpenses db year month)

-- Função auxiliar para a função getCashFlow.
getCashFlow' :: Double -> [Transaction] -> (Double, (Integer, Double))
getCashFlow' balance transactions = (newBalance, (getDay (transactions !! 0), newBalance))
  where newBalance = (getRemains transactions) + balance