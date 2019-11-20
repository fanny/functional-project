module Queries (
  getTransactionsByYear,
  getTransactionsByYearAndMonth,
  getRevenuesByYearAndMonth,
  getExpensesByYearAndMonth,
  getRevenueValueByYearAndMonth,
  getExpenseValueByYearAndMonth,
  getRemainsValueByYearAndMonth,
  getAvgRevenuesByYear,
  getAvgExpensesByYear
) where

import Filters
import Transaction
import Helpers
import JsonParser

getTransactionsByYear :: Integer -> IO [Transaction]
getTransactionsByYear year = do
  transactions <- getTransactions
  return (filterByYear year transactions)

getTransactionsByYearAndMonth :: Integer -> Integer -> IO [Transaction]
getTransactionsByYearAndMonth year month = do
  transactions <- getTransactions
  return (filterByYearAndMonth year month transactions)

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
  return (average revenues)

getAvgExpensesByYear :: Integer -> IO Float
getAvgExpensesByYear year = do
  expenses <- (getExpensesByYear year)
  return ((average expenses) * (-1))
