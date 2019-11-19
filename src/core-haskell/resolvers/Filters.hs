module Filters ( 
  filterByYear,
  filterByYearAndMonth,
  filterByRevenue,
  filterByExpense
) where

import Transaction
import Helpers

filterByYear :: Integer -> [Transaction] -> [Transaction]
filterByYear year transactions = filter (checkYear year) transactions

filterByYearAndMonth :: Integer -> Integer -> [Transaction] -> [Transaction]
filterByYearAndMonth year month transactions = filter (checkMonth month) transactions

filterByRevenue :: [Transaction] -> [Transaction]
filterByRevenue transactions = filter (isRevenue) transactions

filterByExpense :: [Transaction] -> [Transaction]
filterByExpense transactions = filter (isExpense) transactions