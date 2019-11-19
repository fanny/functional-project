module Queries (
  getTransactionsByYear,
  getTransactionsByYearAndMonth,
  getRevenuesByYearAndMonth,
  getExpensesByYearAndMonth,
  getRevenueValueByYearAndMonth,
  getExpenseValueByYearAndMonth,
  getRemainsValueByYearAndMonth
) where

import Filters
import Transaction
import JsonParser

getTransactionsByYear :: Integer -> IO [Transaction]
getTransactionsByYear year = do
  transactions <- getTransactions
  return (filterByYear year transactions)

getTransactionsByYearAndMonth :: Integer -> Integer -> IO [Transaction]
getTransactionsByYearAndMonth year month = do
  transactions <- getTransactions
  return (filterByYearAndMonth year month transactions)

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