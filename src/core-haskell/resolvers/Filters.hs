module Filters ( 
  filterByYear,
  filterByMonth,
  filterByYearAndMonth,
  filterByRevenue,
  filterByExpense,
  filterByRevenueAndExpense
) where

import Transaction
import Helpers

-- Filtra as transações pelo ano.
filterByYear :: Integer -> [Transaction] -> [Transaction]
filterByYear year transactions = filter (checkYear year) transactions

-- Filtra as transações pelo mês.
filterByMonth :: Integer -> [Transaction] -> [Transaction]
filterByMonth month transactions = filter (checkMonth month) transactions

-- Filtra as transações pelo ano e mês.
filterByYearAndMonth :: Integer -> Integer -> [Transaction] -> [Transaction]
filterByYearAndMonth year month transactions = filterByMonth month (filterByYear year transactions)

-- Filtra as transações pelas que são de receita.
filterByRevenue :: [Transaction] -> [Transaction]
filterByRevenue transactions = filter (isRevenue) transactions

-- Filtra as transações pelas que são de despesa.
filterByExpense :: [Transaction] -> [Transaction]
filterByExpense transactions = filter (isExpense) transactions

-- Filtra as transações pelas que são de despesa ou receita.
filterByRevenueAndExpense :: [Transaction] -> [Transaction]
filterByRevenueAndExpense transactions = filter (isRevenueOrExpense) transactions